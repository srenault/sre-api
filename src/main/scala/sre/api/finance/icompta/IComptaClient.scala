package sre.api.finance
package icompta

import cats.effect._
import cats.implicits._
import fs2.Stream
import java.sql.{ DriverManager, Connection }
import anorm._
import sre.api.Settings

case class IComptaClient[F[_]]()(implicit connection: Connection, F: Effect[F]) {

  private def selectRules(): F[List[RuleRecord]] =
    F.delay {
      SQL"SELECT * FROM LARule".as(RuleRecord.parser.*)
    }

  private def selectConditions(): F[List[ConditionRecord]] =
    F.delay {
      SQL"SELECT * FROM LACondition".as(ConditionRecord.parser.*)
    }

  private def selectParameters(): F[List[ParameterRecord]] =
    F.delay {
      SQL"SELECT * FROM LAParameter".as(ParameterRecord.parser.*)
    }

  private def selectAll(): F[Records] =
    for {
      rules <- selectRules()
      conditions <- selectConditions()
      parameters <- selectParameters()
    } yield {
      Records(rules, conditions, parameters)
    }

  def buildRulesAst[A](): F[RuleAst] =
    selectAll().map(RulesAst.build)
}

object IComptaClient {

  def stream[F[_]: ConcurrentEffect](settings: Settings)(implicit F: Effect[F]): Stream[F, IComptaClient[F]] = {
    Class.forName("org.sqlite.JDBC")
    implicit val connection = DriverManager.getConnection(settings.finance.icompta.db)
    Stream.eval(F.pure(IComptaClient()))
  }
}
