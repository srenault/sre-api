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

          val allStatements = segmentsForPeriod.flatMap(_.statements.toList).toList.sorted(CMStatement.ORDER_DESC)

          val partitions = segmentsForPeriod.flatMap(_.partitions)

          NonEmptyList.fromList(segmentsForPeriod) match {
            case Some(segmentsForPeriod) =>

              val updatedAccPeriods: List[PeriodIndex] = maybeLastPeriod match {

                case Some(lastPeriod) =>
                  if (scala.math.abs(ChronoUnit.DAYS.between(wageStatement.date, lastPeriod.wageStatements.last.date)) < 10) { // Include new wage statement
                    val statementsForLastPeriod = allStatements.takeWhile(_.date.isAfter(wageStatement.date.minusDays(1)))

                    val updatedLastPeriod = lastPeriod.includeNewWageStatement(wageStatement, statementsForLastPeriod)

                    updatedLastPeriod :: accPeriods.tail
                  } else { // Complete last complete period (same day) and create a new complete
                    val (statementsForLastPeriod, statementsForPeriod) = allStatements.span(_.date.isEqual(lastPeriod.startDate))

                    val updatedLastPeriod = lastPeriod.includeStatements(statementsForLastPeriod)

                    val newPeriod = CompletePeriodIndex(
                      partitions,
                      startDate = wageStatement.date,
                      endDate = lastPeriod.startDate,
                      wageStatements = NonEmptyList.one(wageStatement),
                    ).includeStatements(statementsForPeriod)

                    newPeriod :: updatedLastPeriod :: accPeriods.tail
                  }

                case None =>
                  val statementsForPeriod = allStatements.takeWhile(st => st.date.isAfter(wageStatement.date.minusDays(1)))
                  val period = IncompletePeriodIndex(
                    partitions,
                    startDate = wageStatement.date,
                    wageStatements = NonEmptyList.one(wageStatement),
                  ).includeStatements(statementsForPeriod)
                  (period :: accPeriods)
              }

              step(restSegments, updatedAccPeriods, pendingSegments = Nil)

            case None =>
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

  private def computeSegmentIndexes(statements: List[CMStatement], partitions: List[OfxFile])(isWageStatement: CMStatement => Boolean): List[SegmentIndex] = {

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

  def buildIndexes(statements: List[CMStatement]): F[List[PeriodIndex]] = {
    icomptaClient.buildRulesAst().map { rulesAst =>
      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).toList.flatMap { wageRuleAst =>
        val segments = computeSegmentIndexes(statements, partitions = Nil)(wageRuleAst.test)
        val (periods, _) = computePeriodIndexes(segments, periods = Nil, pendingSegments = Nil)
        periods
      }.toList
    }
  }

  def buildHistoryIndexesFromScrach(): F[List[PeriodIndex]] = {
    val accountDirs@(accountDir :: _) = settings.finance.accountsDir
    val ofxFiles = OfxDir.listFiles(accountDir)
    buildHistoryIndexes(accountDirs, ofxFiles)
  }

  def buildLastestHistoryIndexes(): F[List[PeriodIndex]] = {
    val accountDirs@(accountDir :: _) = settings.finance.accountsDir
    val ofxFiles = OfxDir.listFiles(accountDir).sortBy(-_.date.toEpochDay).take(4)
    buildHistoryIndexes(accountDirs, ofxFiles)
  }

  def buildHistoryIndexes(accountDirs: List[File], ofxFiles: List[OfxFile]): F[List[PeriodIndex]] = {

    icomptaClient.buildRulesAst().flatMap { rulesAst =>

      rulesAst.traverse(settings.finance.icompta.wageRuleId :: Nil).map { wageRuleAst =>

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

                val sortedStatements = statements.sorted(CMStatement.ORDER_DESC)

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

                val segments = computeSegmentIndexes(nextStatements, partitions)(wageRuleAst.test)

                val (updatedAccPeriods, updatedAccSegments) = computePeriodIndexes(segments, accPeriods, accSegments)

                step(accountDirs, remainingOfxFiles, updatedAccSegments, updatedAccPeriods)
              }

            case Nil =>
              val ascendantPeriods = accPeriods.sorted(PeriodIndex.ORDER_ASC)

              F.pure(ascendantPeriods)
          }
        }

        val sortedOfxFiles = ofxFiles.sortBy(-_.date.toEpochDay)

        step(accountDirs, sortedOfxFiles, accSegments = Nil, accPeriods = Nil)

      }.toList.sequence.map(_.flatten)
    }
  }
}
