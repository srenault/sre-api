package sre.api
package finance
package analytics

import java.io.File
import java.time.temporal.ChronoUnit
import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import icompta.IComptaClient
import cm.CMStatement
import ofx.{ OfxFile, OfxStmTrn, OfxDir }

case class AnalyticsIndex[F[_]](
  icomptaClient: IComptaClient[F],
  dbClient: DBClient[F],
  settings: Settings
)(implicit F: Effect[F]) {

  private def computePeriodIndexes(segments: List[SegmentIndex], periods: List[PeriodIndex], pendingSegments: List[SegmentIndex]): (List[PeriodIndex], List[SegmentIndex]) = {

    @annotation.tailrec
    def step(sortedSegments: List[SegmentIndex], accPeriods: List[PeriodIndex], pendingSegments: List[SegmentIndex]): (List[PeriodIndex], List[SegmentIndex]) = {
      sortedSegments match {
        case (segment@SegmentIndex(_, Some(wageStatement), _)) :: restSegments =>
          val maybeLastPeriod = accPeriods.headOption

          val segmentsForPeriod = (segment +: pendingSegments).distinct

          val allStatements = segmentsForPeriod.flatMap(_.statements.toList).toList.sortBy(-_.date.toEpochDay).distinct

          val partitions = segmentsForPeriod.flatMap(_.partitions)

          NonEmptyList.fromList(segmentsForPeriod) match {
            case Some(segmentsForPeriod) =>

              val updatedAccPeriods = maybeLastPeriod match {
                case Some(lastPeriod) =>
                  if (scala.math.abs(ChronoUnit.DAYS.between(wageStatement.date, lastPeriod.wageStatements.last.date)) < 10) {
                    val statementsForPeriod = allStatements.takeWhile(st => st.date.isAfter(wageStatement.date.minusDays(1)))
                    val balance = statementsForPeriod.foldLeft(0D)(_ + _.amount)
                    val updatedWageStatements = wageStatement :: lastPeriod.wageStatements
                    val updatedBalance = lastPeriod.balance + balance
                    val updatedLastPeriod = lastPeriod.copy(startDate = wageStatement.date, wageStatements = updatedWageStatements, balance = updatedBalance)
                    updatedLastPeriod :: accPeriods.tail
                  } else {
                    val (statementsForLastPeriod, statementsForPeriod) = allStatements.span(st => st.date.isEqual(lastPeriod.startDate))
                    val balanceForLastPeriod = statementsForLastPeriod.foldLeft(0D)(_ + _.amount)
                    val updatedLastPeriod = lastPeriod.copy(balance = lastPeriod.balance + balanceForLastPeriod)
                    val balance = statementsForPeriod.foldLeft(0D)(_ + _.amount)
                    val period = PeriodIndex(partitions, startDate = wageStatement.date, endDate = Some(lastPeriod.startDate), wageStatements = wageStatement :: Nil, balance = balance)
                    period :: updatedLastPeriod :: accPeriods.tail
                  }

                case _ =>
                  val statementsForPeriod = allStatements.takeWhile(st => st.date.isAfter(wageStatement.date.minusDays(1)))
                  val balance = statementsForPeriod.foldLeft(0D)(_ + _.amount)
                  val period = PeriodIndex(partitions, startDate = wageStatement.date, endDate = maybeLastPeriod.map(_.startDate), wageStatements = wageStatement :: Nil, balance = balance)
                  (period :: accPeriods)
              }

              step(restSegments, updatedAccPeriods, pendingSegments = Nil)

            case _ =>
              step(restSegments, accPeriods, pendingSegments = Nil)
          }

        case (segment :: restSegments) =>
          step(restSegments, accPeriods, segment :: pendingSegments)

        case Nil =>
          (accPeriods, pendingSegments)
      }
    }

    val sortedSegments = segments.sortBy(-_.startDate.toEpochDay)

    step(sortedSegments, accPeriods = periods, pendingSegments)
  }

  private def computeSegmentIndexes(statements: List[CMStatement], partitions: List[OfxFile])(isWageStatement: CMStatement => Boolean): List[SegmentIndex] = {

    @annotation.tailrec
    def step(statements: List[CMStatement], acc: List[SegmentIndex]): List[SegmentIndex] = {
      statements match {
        case Nil => acc

        case statements =>

          val sortedStatements = statements.sortBy(-_.date.toEpochDay);

          val maybeWageStatement = sortedStatements.find(isWageStatement)

          maybeWageStatement match {
            case Some(wageStatement) =>
              val (statementsForPeriod, remainingStatements) = {
                val (in, out) = sortedStatements.span(_ != wageStatement)
                (in ++  out.headOption.toList, out.tail)
              }
              val updatedAcc = SegmentIndex(partitions, Some(wageStatement), statementsForPeriod) :: acc
              step(remainingStatements, updatedAcc)

            case _ =>
              SegmentIndex(partitions, wageStatement = None, sortedStatements) :: acc
          }
      }
    }

    step(statements, acc = Nil).reverse
  }

  def buildIndexes(statements: List[CMStatement]): F[List[PeriodIndex]] = {
    icomptaClient.buildRulesAst().map { rulesAst =>
      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).toList.flatMap { wageRuleAst =>
        val segments = computeSegmentIndexes(statements, partitions = Nil)(wageRuleAst.test)
        val (periods, _) = computePeriodIndexes(segments, periods = Nil, pendingSegments = Nil)
        periods
      }.toList
    }
  }

  def buildHistoryIndexes(): F[List[PeriodIndex]] = {

    icomptaClient.buildRulesAst().flatMap { rulesAst =>

      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).map { wageRuleAst =>

        val accountDirs@(accountDir :: otherAccountDirs) = settings.finance.transactionsDir.listFiles.toList.filter(_.isDirectory)

        def step(sortedOfxFiles: List[OfxFile], accSegments: List[SegmentIndex], accPeriods: List[PeriodIndex]): F[List[PeriodIndex]] = {
          sortedOfxFiles match {
            case ofxFile :: remainingOfxFiles =>
              val partitions: List[OfxFile] = accountDirs.map { accountDir =>
                new File(accountDir.getPath + "/" + ofxFile.name)
              }.flatMap(OfxFile.fromFile)

              partitions.map(p => OfxStmTrn.load(p).map(p -> _)).sequence.map {
                _.flatMap { case (partition, transactions) =>
                  transactions.map(_.toStatement(partition))
                }
              }.flatMap { statements =>

                val sortedStatements = statements.sortBy(-_.date.toEpochDay)

                val maybeLastWageStatement = accPeriods.headOption.flatMap(period => period.wageStatements.sortBy(_.date.toEpochDay).headOption)

                val nextStatements = maybeLastWageStatement match {
                  case Some(lastWageStatement) =>
                    sortedStatements.dropWhile { st =>
                      st.date.isAfter(lastWageStatement.date) || (st.date.isEqual(lastWageStatement.date) && st.id != lastWageStatement.id)
                    }.dropWhile(st => st.id == lastWageStatement.id)

                  case None => statements
                }

                val segments = computeSegmentIndexes(nextStatements, partitions)(wageRuleAst.test)

                val (periods, pendingSegments) = computePeriodIndexes(segments, accPeriods, accSegments)

                step(remainingOfxFiles, pendingSegments, periods)
              }

            case Nil =>
              val periods = if (accSegments.isEmpty) accPeriods else {
                val pendingStatements = accSegments.flatMap(_.statements).sortBy(-_.date.toEpochDay)

                (for {
                  lastPeriod <- accPeriods.headOption
                  wageStatement <- lastPeriod.wageStatements.sortBy(_.date.toEpochDay).headOption
                } yield {
                  val pendingStatementsForPeriod = pendingStatements.takeWhile(st => st.date.isBefore(wageStatement.date.minusDays(1)))
                  val pendingBalance = pendingStatementsForPeriod.foldLeft(0D)(_ + _.amount)
                  lastPeriod.copy(balance = lastPeriod.balance + pendingBalance) :: accPeriods.tail
                }) getOrElse accPeriods
              }

              F.pure(periods.sortBy(_.startDate.toEpochDay))
          }
        }

        val sortedOfxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay)//.filter(f => List("2019-09-05.ofx", "2019-08-02.ofx").exists(f.name == _))

        step(sortedOfxFiles, accSegments = Nil, accPeriods = Nil)

      }.toList.sequence.map(_.flatten)
    }
  }
}
