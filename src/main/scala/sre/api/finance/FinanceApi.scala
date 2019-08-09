package sre.api.finance

import cats.effect._
import cats.data.OptionT
import cats.implicits._
import sre.api.Settings

case class FinanceApi[F[_]](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings)(implicit F: Effect[F]) {

  private def withRecordsAst[A](f: RuleAst => A): F[A] = {
    icomptaClient.selectAll().map { records =>
      val rulesAst = RulesAst.build(records)
      f(rulesAst)
    }
  }

  def computeExpensesByCategory(account: CMAccount): OptionT[F, List[ExpensesByCategory]] = {
    OptionT.liftF(withRecordsAst { rulesAst =>
      val categories = settings.finance.cm.accounts
        .find(_.id == account.id)
        .map(_.categories)
        .getOrElse(Map.empty)

      categories.flatMap {
        case (categoryId, category) =>
          rulesAst.traverse(category.path) map { ruleAst =>
            val amount = account.statements.foldLeft(0F) { (acc, statement) =>
              if (ruleAst.test(statement)) {
                acc + scala.math.abs(statement.amount)
              } else {
                acc
              }
            }
            ExpensesByCategory(categoryId, category.label, amount, category.threshold)
          }
      }.toList
    })
  }

  def computeLastPeriod(statements: List[CMStatement]): F[Option[CMPeriod]] = {
    // computePeriodSegments(statements).map(_.lastOption).map { maybeSegment =>
    //   for {
    //     segment <- maybeSegment
    //     wageDate <- segment.wageDate
    //   } yield {
    //     CMPeriod(startDate = wageDate, endDate = None, segment.statements)
    //   }
    // }
    ???
  }

  def computePeriods(sortedSegments: List[CMPeriodSegment]): (List[CMPeriod], List[CMPeriodSegment]) = {
    @annotation.tailrec
    def step(sortedSegments: List[CMPeriodSegment], accPeriods: List[CMPeriod], unlinkedSegments: List[CMPeriodSegment]): (List[CMPeriod], List[CMPeriodSegment]) = {
      sortedSegments match {
        case (CMPeriodSegment(Some(wageDate), statements) :: restSegments) =>
          val allStatements = (statements ::: unlinkedSegments.flatMap(_.statements)).distinct.sortBy(-_.date.toEpochDay)
          //println("++++++++++++++ ALL STATEMENTS")
          //allStatements.foreach(println)
          val endDate = allStatements.headOption.map(_.date)
          val period = CMPeriod(startDate = wageDate, endDate = endDate, allStatements)
          step(restSegments, period :: accPeriods, unlinkedSegments = Nil)

        case (segment :: restSegments) =>
          step(restSegments, accPeriods, segment :: unlinkedSegments)

        case Nil => (accPeriods, unlinkedSegments)
      }
    }

    step(sortedSegments, accPeriods = Nil, unlinkedSegments = Nil)
  }

  def computePeriodSegments(wageTransactions: List[ScheduledTransaction], statements: List[CMStatement]): List[CMPeriodSegment] = {
    @annotation.tailrec
    def step(wageTransactions: List[ScheduledTransaction], sortedStatements: List[CMStatement], acc: List[CMPeriodSegment]): List[CMPeriodSegment] = {
      sortedStatements match {
        case Nil => acc

        case sortedStatements =>

          val maybeWageStatement = sortedStatements.sortBy(-_.date.toEpochDay).filter { statement =>
            wageTransactions.exists(wageTransaction => statement.label.startsWith(wageTransaction.name))
          }.take(2).lastOption

          maybeWageStatement match {
            case Some(wageStatement) =>
              val (statementsForPeriod, remainingStatements) = sortedStatements.span(statement => !statement.date.isBefore(wageStatement.date))
              val maybeWageDate = statementsForPeriod.lastOption.filter(_.date.isEqual(wageStatement.date)).map(_.date)
              val updatedAcc = CMPeriodSegment(maybeWageDate, statementsForPeriod) :: acc
              step(wageTransactions, remainingStatements, updatedAcc)

            case _ =>
              (CMPeriodSegment(None, sortedStatements) :: acc).reverse

          }
      }
    }

    val sortedStatements = statements.sortBy(-_.date.toEpochDay)

    step(wageTransactions, sortedStatements, acc = Nil)
  }

  def f() = {
    import java.io.File

    val categoryLabel = settings.finance.icompta.wageCategory
    icomptaClient.selectScheduledTransactionsByCategoryLabel(categoryLabel).flatMap { wageTransactions =>

      val transactionsDir = new java.io.File("/Volumes/data/srebox/credit_mutuel/transactions")
      val accountDirs@(accountDir :: otherAccountDirs) = transactionsDir.listFiles.toList.filter(_.isDirectory)

      def step(sortedOfxFiles: List[OfxFile], acc: List[CMPeriodSegment]): F[Unit] = {
        sortedOfxFiles match {
          case ofxFile :: remainingOfxFiles =>
            println("###> FILE " + ofxFile)
            val eventuallyTransactions: F[List[OfxStmTrn]] = accountDirs.map { accountDir =>
              val f = new File(accountDir.getPath + "/" + ofxFile.name)
              OfxStmTrn.load(f)
            }.sequence.map(_.flatten)


            eventuallyTransactions.flatMap { transactions =>
              val statements = transactions.map(_.toStatement)
              //println("\n--------------------------------------------------------------------------")
              //println("\n** STATEMENTS **")
              //statements.sortBy(-_.date.toEpochDay).foreach(s => println(s"#>> ${s}"))

              val segments = computePeriodSegments(wageTransactions, statements)
              //println("\n** SEGMENTS **")
              // segments.foreach { s =>
              //   println(s"#>>>>>>>>>>>>>>>>>>> ${s.wageDate}")
              //   s.statements.foreach(println)
              //   println("<<<<<<<<<<<<<<<<<<<<<")
              // }

              val (periods, remainingSegments) = computePeriods(acc ::: segments)
              //println("\n** PERIODS **")
              //periods.foreach(p => println(s"#>> ${p.startDate} ${p.endDate}"))

              //println("\n** REMAINING SEGMENTS **")
              //remainingSegments.foreach(r => println(s"#>> ${r}"))

              step(remainingOfxFiles, remainingSegments)
            }

          case Nil =>
            F.pure(println("END"))
        }
      }

      val sortedOfxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay).take(3)

      step(sortedOfxFiles, acc = Nil)
      // OfxStmTrn.streamOfxDir(accountDir).fold(List.empty[CMPeriodSegment]) {
      //   case (acc, transactions) =>
      //     println("\n--------------------------------------------------------------------------")
      //     println("\n** TRANSACTIONS **")
      //     transactions.foreach(t => println(s"#>> ${t}"))

      //     val segments = computePeriodSegments(wageTransactions, transactions.map(_.toStatement))
      //     println("\n** SEGMENTS **")
      //     segments.foreach(s => println(s"#>> ${s}"))

      //     val (periods, remainingSegments) = computePeriods(acc ::: segments)
      //     println("\n** PERIODS **")
      //     periods.foreach(p => println(s"#>> ${p}"))

      //     println("\n** REMAINING SEGMENTS **")
      //     remainingSegments.foreach(r => println(s"#>> ${r}"))

      //     remainingSegments
      // }.compile.drain
    }
  }
}
