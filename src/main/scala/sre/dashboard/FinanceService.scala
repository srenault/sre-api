package sre.dashboard

import java.time.LocalDate
import cats.data.Validated.{ Invalid, Valid }
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import io.circe.literal._
import finance._

class FinanceService[F[_]: Effect](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings) extends FinanceServiceDsl[F] {

  val financeApi = FinanceApi(icomptaClient, cmClient, settings)

  val service: HttpService[F] = {
    HttpService[F] {

      case GET -> Root / "accounts" =>

        for {
          accounts <- cmClient.fetchAccounts()

          statements <- financeApi.fetchStatementsForPeriod(accounts, LocalDate.now)

          (credit, debit) = financeApi.computeCreditAndDebit(statements)

          startPeriod = statements.headOption.map(_.date)

          res <- Ok(json"""{ "startPeriod": $startPeriod, "credit": $credit, "debit": $debit, "accounts": $accounts }""")
        } yield res

      case GET -> Root / "accounts" / AccountIdVar(accountId) / "statements" =>
        cmClient.fetchStatements(accountId).flatMap { statements =>
          Ok(statements)
        }

      case GET -> Root / "accounts" / AccountIdVar(accountId) / "expenses" :? DateQueryParamMatcher(maybeValidatedDate) =>
        val date = maybeValidatedDate match {
          case Some(Invalid(e)) => Left(e)
          case Some(Valid(date)) => Right(date)
          case None => Right(LocalDate.now.withDayOfMonth(1))
        }

        date match {
          case Left(e) => BadRequest(s"Invalid date: $e")

          case Right(date) =>
            for {
              transactions <- cmClient.fetchStatements(accountId, maybeStartDate = Some(date))
              maybeAmount <- financeApi.computeExpensesByCategory(accountId, transactions, date).value
              (credit, debit) = financeApi.computeCreditAndDebit(transactions)
              res <- maybeAmount match {
                case Some(result) =>
                  Ok(result)

                case None => NotFound()
              }
            } yield res
        }
    }
  }
}
