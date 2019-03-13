package sre.dashboard

import java.time.LocalDate
import io.circe.syntax._
import cats.data.Validated.{Invalid, Valid}
import cats.effect._
import cats.implicits._
import org.http4s._
import finance._

class FinanceService[F[_]: Effect](icomptaClient: IComptaClient[F], cmClient: CMClient[F], settings: Settings) extends FinanceServiceDsl[F] {

  val financeApi = FinanceApi(icomptaClient, settings: Settings)

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root / "expenses" :? DateQueryParamMatcher(maybeValidatedDate) =>
        val date = maybeValidatedDate match {
          case Some(Invalid(e)) =>
            Left(e)

          case Some(Valid(date)) =>
            Right(date)

          case None => // fallback
            Right(LocalDate.now)
        }

        date match {
          case Left(_) =>
            BadRequest()

          case Right(date) =>
            for {
              transactions <- cmClient.exportAsOfx(settings.finance.icompta.accountId)
              maybeAmount <- financeApi.computeExpensesByCategory(transactions, date).value
              res <- maybeAmount match {
                case Some(res) => Ok(res.asJson.noSpaces)
                case None => NotFound()
              }
            } yield res
        }

      case GET -> Root / "accounts" =>
        cmClient.fetchAccounts().flatMap { accounts =>
          Ok(accounts)
        }
    }
  }
}
