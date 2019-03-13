package sre.dashboard.finance

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.sql.{ DriverManager, Connection }
import anorm._
import sre.dashboard.Settings

case class IComptaClient[F[_]: Effect](implicit connection: Connection) {

  def selectRules()(implicit F: Effect[F]): F[List[RuleRecord]] =
    F.pure {
      SQL"SELECT * FROM LARule".as(RuleRecord.parser.*)
    }

  def selectConditions()(implicit F: Effect[F]): F[List[ConditionRecord]] =
    F.pure {
      SQL"SELECT * FROM LACondition".as(ConditionRecord.parser.*)
    }

  def selectParameters()(implicit F: Effect[F]): F[List[ParameterRecord]] =
    F.pure {
      SQL"SELECT * FROM LAParameter".as(ParameterRecord.parser.*)
    }

  def selectAll(): F[Records] =
    for {
      rules <- selectRules()
      conditions <- selectConditions()
      parameters <- selectParameters()
    } yield {
      Records(rules, conditions, parameters)
    }
}

object IComptaClient {

  def stream[F[_]: ConcurrentEffect](settings: Settings)(implicit F: Effect[F]): Stream[F, IComptaClient[F]] = {
    Class.forName("org.sqlite.JDBC")
    implicit val connection = DriverManager.getConnection(settings.finance.icompta.db)
    Stream.eval(F.pure(IComptaClient()))
  }
}
