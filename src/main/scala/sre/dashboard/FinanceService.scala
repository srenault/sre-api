package sre.dashboard

import java.time.LocalDate
import cats.data.Validated.{ Invalid, Valid }
import cats.effect._
import cats.implicits._
import org.http4s._
import finance._

class FinanceService[F[_]: Effect](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings) extends FinanceServiceDsl[F] {

  implicit val s: Settings = settings

  val financeApi = FinanceApi(icomptaClient, settings)

  val service: HttpService[F] = {
    HttpService[F] {

      case GET -> Root / "accounts" =>
        cmClient.fetchAccounts().flatMap { accounts =>
          Ok(accounts)
        }

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
              transactions <- cmClient.exportAsOfx(accountId, maybeStartDate = Some(date))
              maybeAmount <- financeApi.computeExpensesByCategory(transactions, date).value
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
