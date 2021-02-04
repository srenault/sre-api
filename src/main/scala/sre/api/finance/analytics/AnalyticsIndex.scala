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

case class AnalyticsIndexClient[F[_]](
  icomptaClient: IComptaClient[F],
  settings: Settings
)(implicit F: Effect[F]) {

  def computePeriodIndexesFrom(statements: List[CMStatement]): F[List[PeriodIndex]] = {
    icomptaClient.buildRulesAst().map { rulesAst =>
      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).toList.flatMap { wageRuleAst =>
        val segments = AnalyticsIndexClient.computeSegmentIndexesStep(statements, partitions = Nil)(wageRuleAst.test)
        val (periods, _) = AnalyticsIndexClient.computePeriodIndexesStep(segments, periods = Nil, pendingSegments = Nil)
        periods
      }.toList
    }
  }

  def computePeriodIndexesFromScratch(): F[List[PeriodIndex]] =
    settings.finance.accountsDir match {
      case accountDirs@(accountDir :: _) =>
        val ofxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay)
        computePeriodIndexes(accountDirs, ofxFiles)

      case _ => F.pure(Nil)
    }

  def computeLatestPeriodIndexes(nmonths: Int = 4): F[List[PeriodIndex]] =
    settings.finance.accountsDir match {
      case accountDirs@(accountDir :: _) =>
        val ofxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay).take(nmonths)
        computePeriodIndexes(accountDirs, ofxFiles)

      case _ => F.pure(Nil)
    }

  private def computePeriodIndexes(accountDirs: List[File], ofxFiles: List[OfxFile]): F[List[PeriodIndex]] = {
    icomptaClient.buildRulesAst().flatMap { rulesAst =>

      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).map { wageRule =>

        AnalyticsIndexClient.computePeriodIndexes(accountDirs, ofxFiles)(wageRule.test)

      }.toList.sequence.map(_.flatten.filter(_.isValid))
    }
  }
}

object AnalyticsIndexClient {

  private def computePeriodIndexesStep(segments: List[SegmentIndex], periods: List[PeriodIndex], pendingSegments: List[SegmentIndex]): (List[PeriodIndex], List[SegmentIndex]) = {

    @annotation.tailrec
    def step(sortedSegments: List[SegmentIndex], accPeriods: List[PeriodIndex], pendingSegments: List[SegmentIndex]): (List[PeriodIndex], List[SegmentIndex]) = {
      val maybeLastPeriod = accPeriods.headOption

      sortedSegments match {
        case (segment@SegmentIndex(_, Some(wageStatement), _)) :: restSegments =>
          val segmentsForPeriod = (segment +: pendingSegments).distinct

          val allStatements = CMStatement.merge(segmentsForPeriod.flatMap(_.statements.toList))
            .sorted(CMStatement.ORDER_DESC)

          val statementsForPeriod = allStatements.takeWhile(_.date.isAfter(wageStatement.date.minusDays(1)))

          val partitions = segmentsForPeriod.flatMap(_.partitions)

          if (segmentsForPeriod.nonEmpty) {

            val updatedAccPeriods: List[PeriodIndex] = maybeLastPeriod match {

              case Some(lastPeriod) =>
                if (scala.math.abs(ChronoUnit.DAYS.between(wageStatement.date, lastPeriod.wageStatements.last.date)) <= 10) { // Include new wage statement
                  val updatedLastPeriod = lastPeriod.includeNewWageStatement(wageStatement, statementsForPeriod, partitions)
                  updatedLastPeriod :: accPeriods.tail
                } else { // Complete last complete period (same day) and create a new complete

                  val (statementsForLastPeriod, statementsForCurrentPeriod) = statementsForPeriod.span(_.date.isEqual(lastPeriod.startDate))

                  val updatedLastPeriod = lastPeriod.includeStatements(statementsForLastPeriod, partitions)

                  val newPeriod = CompletePeriodIndex(
                    partitions,
                    startDate = wageStatement.date,
                    endDate = lastPeriod.startDate,
                    wageStatements = NonEmptyList.one(wageStatement),
                  ).includeStatements(statementsForCurrentPeriod)

                  newPeriod :: updatedLastPeriod :: accPeriods.tail
                }

              case None =>
                val period = IncompletePeriodIndex(
                  partitions,
                  startDate = wageStatement.date,
                  wageStatements = NonEmptyList.one(wageStatement),
                ).includeStatements(statementsForPeriod)

                period :: accPeriods
            }

            step(restSegments, updatedAccPeriods, pendingSegments = Nil)
          } else {
            step(restSegments, accPeriods, pendingSegments = Nil)
          }

        case (segment :: restSegments) =>
          step(restSegments, accPeriods, segment :: pendingSegments)

        case Nil =>
          (accPeriods, pendingSegments)
      }
    }

    val sortedSegments = segments.sorted(SegmentIndex.ORDER_DESC)

    step(sortedSegments, accPeriods = periods, pendingSegments)
  }

  private def computeSegmentIndexesStep(statements: List[CMStatement], partitions: List[OfxFile])(isWageStatement: CMStatement => Boolean): List[SegmentIndex] = {

    @annotation.tailrec
    def step(statements: List[CMStatement], acc: List[SegmentIndex]): List[SegmentIndex] = {
      statements match {
        case Nil => acc

        case statements =>

          val sortedStatements = statements.sorted(CMStatement.ORDER_DESC)

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

  def computePeriodIndexes[F[_]](accountDirs: List[File], ofxFiles: List[OfxFile])(isWageStatement: CMStatement => Boolean)(implicit F: Effect[F]): F[List[PeriodIndex]] = {

    def step(accountDirs: List[File], sortedOfxFiles: List[OfxFile], accSegments: List[SegmentIndex], accPeriods: List[PeriodIndex]): F[List[PeriodIndex]] = {
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

            val sortedStatements = CMStatement.merge(statements).sorted(CMStatement.ORDER_DESC)

            val maybeLastWageStatement = accPeriods.headOption.map(period => period.startWageStatement)

            // Remove already processed statements
            val nextStatements = maybeLastWageStatement match {
              case Some(lastWageStatement) =>
                sortedStatements.dropWhile { st =>
                  st.date.isAfter(lastWageStatement.date) ||
                  (st.date.isEqual(lastWageStatement.date) && st.id != lastWageStatement.id)
                }.dropWhile(_.id == lastWageStatement.id)

              case None => statements
            }

            val segments = computeSegmentIndexesStep(nextStatements, partitions)(isWageStatement)

            val (updatedAccPeriods, updatedAccSegments) = computePeriodIndexesStep(segments, accPeriods, accSegments)

            step(accountDirs, remainingOfxFiles, updatedAccSegments, updatedAccPeriods)
          }

        case Nil =>

          val periods = accPeriods.headOption match {
            case Some(lastPeriod) => // Complete the last period with remaining statements if needed
              val remainingSegments = accSegments.takeWhile(segment => segment.statements.exists(_.date == lastPeriod.startDate))
              val remainingStatements = CMStatement.merge(remainingSegments.flatMap(_.statements).toList).sorted(CMStatement.ORDER_DESC)
              val statementsForLastPeriod = remainingStatements.takeWhile(_.date.isEqual(lastPeriod.startDate))
              val partitions = remainingSegments.flatMap(_.partitions)
              val updatedLastPeriod = lastPeriod.includeStatements(statementsForLastPeriod, partitions)
              updatedLastPeriod :: accPeriods.tail

            case None =>
              accPeriods
          }

          val ascendantPeriods = periods.sorted(PeriodIndex.ORDER_ASC)

          F.pure(ascendantPeriods)
      }
    }

    val sortedOfxFiles = ofxFiles.sortBy(-_.date.toEpochDay)

    step(accountDirs, sortedOfxFiles, accSegments = Nil, accPeriods = Nil)

  }
}
