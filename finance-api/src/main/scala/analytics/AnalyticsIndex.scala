package sre.api.finance
package analytics

import java.time.YearMonth
import java.io.File
import java.time.temporal.ChronoUnit
import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import sre.api.settings.FinanceSettings
import cm.CMStatement
import ofx.{ OfxFile, OfxStmTrn, OfxDir }

case class AnalyticsIndexClient[F[_]](
  dbClient: DBClient[F],
  settings: FinanceSettings
)(implicit F: Sync[F]) {

  private def isWageStatement(statement: CMStatement): Boolean = {
    settings.wageStatements.exists(statement.label.contains _)
  }

  def computePeriodIndexesFrom(statements: List[CMStatement]): List[PeriodIndex] = {
    val segments = AnalyticsIndexClient.computeSegmentIndexesStep(statements, partitions = Nil)(isWageStatement)
    val (periods, _) = AnalyticsIndexClient.computePeriodIndexesStep(segments, periods = Nil, pendingSegments = Nil)
    periods
  }

  def computePeriodIndexesFromScratch(): F[List[PeriodIndex]] =
    settings.accountsDir match {
      case accountDirs@(accountDir :: _) =>
        val ofxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay)
        computePeriodIndexes(accountDirs, ofxFiles)

      case _ => F.pure(Nil)
    }

  def computeLatestPeriodIndexes(nmonths: Int = 4): F[List[PeriodIndex]] =
    settings.accountsDir match {
      case accountDirs@(accountDir :: _) =>
        val ofxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay).take(nmonths)
        ofxFiles.lastOption match {
          case Some(lastOfxFile) =>
            val yearMonth = YearMonth.from(lastOfxFile.date.minusMonths(1))
            dbClient.selectOnePeriodIndex(yearMonth).flatMap { seedPeriod =>
              computePeriodIndexes(accountDirs, ofxFiles, seedPeriod)
            }

          case None => F.pure(Nil)
        }

      case _ => F.pure(Nil)
    }

  private def computePeriodIndexes(accountDirs: List[File], ofxFiles: List[OfxFile], seed: Option[PeriodIndex] = None): F[List[PeriodIndex]] = {
    AnalyticsIndexClient.computePeriodIndexes(accountDirs, ofxFiles, seed)(isWageStatement).map(_.filter(_.isValid))
  }
}

object AnalyticsIndexClient {

  private def computePeriodIndexesStep(segments: List[SegmentIndex], periods: List[PeriodIndex], pendingSegments: List[SegmentIndex]): (List[PeriodIndex], List[SegmentIndex]) = {

    @annotation.tailrec
    def step(sortedSegments: List[SegmentIndex], accPeriods: List[PeriodIndex], pendingSegments: List[SegmentIndex], lastIncludedStatements: List[CMStatement]): (List[PeriodIndex], List[SegmentIndex]) = {
      val maybeLastPeriod = accPeriods.headOption

      sortedSegments match {
        case (segment@SegmentIndex(_, Some(wageStatement), _)) :: restSegments =>
          val segmentsForPeriod = (segment +: pendingSegments).distinct

          val allStatements = CMStatement.merge(segmentsForPeriod.flatMap(_.statements.toList))
            .sorted(CMStatement.ORDER_DESC)

          val statementsForPeriod = allStatements.takeWhile(_.date.isAfter(wageStatement.date.minusDays(1)))

          val partitions = segmentsForPeriod.flatMap(_.partitions)

          if (segmentsForPeriod.nonEmpty) {

            val (updatedAccPeriods, updatedLastIncludedStatements) = maybeLastPeriod match {

              case Some(lastPeriod) =>
                if (scala.math.abs(ChronoUnit.DAYS.between(wageStatement.date, lastPeriod.wageStatements.last.date)) <= 10) { // Include new wage statement
                  val newStatements = statementsForPeriod.filterNot(s1 => lastIncludedStatements.exists(s2 => s1.id == s2.id))
                  val updatedLastPeriod = lastPeriod.includeNewWageStatement(wageStatement, newStatements, partitions)
                  (updatedLastPeriod :: accPeriods.tail) -> newStatements
                } else { // Complete last complete period (same day) and create a new complete

                  val (statementsForLastPeriod, statementsForCurrentPeriod) = statementsForPeriod.span(_.date.isEqual(lastPeriod.startDate))

                  val updatedLastPeriod = lastPeriod
                    .includeStatements(statementsForLastPeriod, partitions)

                  val newPeriod = CompletePeriodIndex(
                    partitions,
                    startDate = wageStatement.date,
                    endDate = lastPeriod.startDate,
                    wageStatements = NonEmptyList.one(wageStatement),
                  ).includeStatements(statementsForCurrentPeriod)

                  (newPeriod :: updatedLastPeriod :: accPeriods.tail) -> statementsForCurrentPeriod
                }

              case None =>
                val period = IncompletePeriodIndex(
                  partitions,
                  startDate = wageStatement.date,
                  wageStatements = NonEmptyList.one(wageStatement),
                ).includeStatements(statementsForPeriod)

                (period :: accPeriods) -> statementsForPeriod
            }

            step(restSegments, updatedAccPeriods, pendingSegments = Nil, updatedLastIncludedStatements)
          } else {
            step(restSegments, accPeriods, pendingSegments = Nil, lastIncludedStatements)
          }

        case (segment :: restSegments) =>
          step(restSegments, accPeriods, segment :: pendingSegments, lastIncludedStatements)

        case Nil =>
          (accPeriods, pendingSegments)
      }
    }

    val sortedSegments = segments.sorted(SegmentIndex.ORDER_DESC)

    step(sortedSegments, accPeriods = periods, pendingSegments, lastIncludedStatements = Nil)
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

  private def fillMissingBalancesByAccount(periodIndexes: List[PeriodIndex]): List[PeriodIndex] = {
    periodIndexes.foldLeft(List.empty[PeriodIndex]) {
      case (acc, periodIndex) =>
        acc.lastOption match {
          case Some(previousPeriodIndex) =>
            val updatedBalancesByAccount = previousPeriodIndex.balancesByAccount ++ periodIndex.balancesByAccount
            val updatedPeriodIndex: PeriodIndex = periodIndex match {
              case completePeriod: CompletePeriodIndex =>
                completePeriod.copy(balancesByAccount = updatedBalancesByAccount)

              case incompletePeriod: IncompletePeriodIndex =>
                incompletePeriod.copy(balancesByAccount = updatedBalancesByAccount)
            }

            acc :+ updatedPeriodIndex

          case None =>
            acc :+ periodIndex
        }
    }
  }

  def computePeriodIndexes[F[_]](accountDirs: List[File], ofxFiles: List[OfxFile], seed: Option[PeriodIndex] = None)(isWageStatement: CMStatement => Boolean)(implicit F: Sync[F]): F[List[PeriodIndex]] = {

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

            val maybeLastPeriod = accPeriods.headOption

            // Remove already processed statements
            val nextStatements = maybeLastPeriod match {
              case Some(lastPeriod) =>
                sortedStatements.dropWhile { st =>
                  st.date.isAfter(lastPeriod.startWageStatement.date) ||
                  (st.date.isEqual(lastPeriod.startWageStatement.date) && !lastPeriod.isWageStatement(st))
                }.dropWhile(st => lastPeriod.isWageStatement(st))

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

    step(accountDirs, sortedOfxFiles, accSegments = Nil, accPeriods = seed.toList).map { result =>

      fillMissingBalancesByAccount(result)

    }
  }
}
