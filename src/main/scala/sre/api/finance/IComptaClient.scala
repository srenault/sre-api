package sre.api.finance

import cats.effect._
import cats.implicits._
import cats.data.OptionT
import fs2.Stream
import java.sql.{ DriverManager, Connection }
import anorm._
import sre.api.Settings

case class IComptaClient[F[_]](implicit connection: Connection, F: Effect[F]) {

  def selectRules(): F[List[RuleRecord]] =
    F.pure {
      SQL"SELECT * FROM LARule".as(RuleRecord.parser.*)
    }

  def selectConditions(): F[List[ConditionRecord]] =
    F.pure {
      SQL"SELECT * FROM LACondition".as(ConditionRecord.parser.*)
    }

  def selectParameters(): F[List[ParameterRecord]] =
    F.pure {
      SQL"SELECT * FROM LAParameter".as(ParameterRecord.parser.*)
    }

  def selectCategoryByLabel(label: String): OptionT[F, CategoryRecord] =
    OptionT.fromOption[F] {
      SQL("SELECT * FROM ICCategory WHERE name = {label}")
        .on("label" -> label)
        .as(CategoryRecord.parser.singleOpt)
    }

  def selectScheduledTransactionsSplitByCategoryId(categoryId: String): F[List[ScheduledTransactionSplitRecord]] =
    F.pure {
      SQL("SELECT * FROM ICScheduledTransactionSplit WHERE category = {category}")
        .on("category" -> categoryId)
        .as(ScheduledTransactionSplitRecord.parser.*)
    }

  def selectScheduledTransactionsByIds(ids: Set[String]): F[List[ScheduledTransactionRecord]] =
    F.pure {
      SQL"SELECT * FROM ICScheduledTransaction WHERE ID IN ($ids)"
        .as(ScheduledTransactionRecord.parser.*)
    }

  def selectScheduledTransactionsByCategoryLabel(categoryLabel: String): F[List[ScheduledTransaction]] = {
    selectCategoryByLabel(categoryLabel).flatMap { category =>
      OptionT.liftF {
        for {
          transactionsSplit <- selectScheduledTransactionsSplitByCategoryId(category.ID)
          transactionIds = transactionsSplit.map(_.transaction).toSet
          transactions <- selectScheduledTransactionsByIds(transactionIds)
        } yield {
          transactions.zip(transactionsSplit).map {
            case (transaction, transactionSplit) =>
              ScheduledTransaction(transaction.name, transactionSplit.amount, transaction.date)
          }
        }
      }
    } getOrElse Nil
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
