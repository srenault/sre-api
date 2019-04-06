package sre.dashboard.finance

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import anorm._

case class Records(rules: List[RuleRecord], conditions: List[ConditionRecord], parameters: List[ParameterRecord]) {
  def parameterOrFail(id: String): ParameterRecord =
    parameters.find(_.ID == id) match {
      case Some(parameter) => parameter
      case None => sys.error(s"Unable to get parameter $id")
    }

  def conditionOrFail(id: String): ConditionRecord =
    conditions.find(_.ID == id) match {
      case Some(condition) => condition
      case None => sys.error(s"Unable to get condition $id")
    }
}

// - Rules
case class RulesGroupRecord(ID: String, name: String, active: Int, parent: String) extends RuleRecord
object RulesGroupRecord {
  implicit lazy val parser = Macro.namedParser[RulesGroupRecord]
}

case class SingleRuleRecord(ID: String, name: String, active: Int, parent: String, condition: String) extends RuleRecord
object SingleRuleRecord {
  implicit lazy val parser = Macro.namedParser[SingleRuleRecord]
}

sealed trait RuleRecord {
  def ID: String
  def parent: String
}

object RuleRecord {
  implicit lazy val parser: RowParser[RuleRecord] = SqlParser.str("class").flatMap {
    case "LARulesGroup" => implicitly[RowParser[RulesGroupRecord]]
    case "LARule" => implicitly[RowParser[SingleRuleRecord]]
    case d => RowParser.failed[RuleRecord](Error(SqlMappingError(
      "unexpected row type \'%s\'; expected: %s".format(d, "LARulesGroup, LARule"))
    ))
  }
}

// - Condition
case class CompoundConditionRecord(ID: String, index: Int, `type`: Int, parent: Option[String]) extends ConditionRecord

object CompoundConditionRecord {
  implicit lazy val parser = Macro.namedParser[CompoundConditionRecord]
}

case class ComparisonConditionRecord(
  ID: String,
  parent: Option[String],
  index: Int,
  keypath: String,
  operator: String,
  parameter: String
) extends ConditionRecord

object ComparisonConditionRecord {
  implicit lazy val parser = Macro.namedParser[ComparisonConditionRecord]
}

sealed trait ConditionRecord {
  def ID: String
  def parent: Option[String]
}

object ConditionRecord {
  implicit lazy val parser: RowParser[ConditionRecord] = SqlParser.str("class").flatMap {
    case "LACompoundCondition" => implicitly[RowParser[CompoundConditionRecord]]
    case "LAComparisonCondition" => implicitly[RowParser[ComparisonConditionRecord]]
    case d => RowParser.failed[ConditionRecord](Error(SqlMappingError(
      "unexpected row type \'%s\'; expected: %s".format(d, "LACompoundCondition, LAComparisonCondition"))
    ))
  }
}

// - Parameter
case class StringParameterRecord(ID: String, string: String) extends ParameterRecord
object StringParameterRecord {
  implicit lazy val parser = Macro.namedParser[StringParameterRecord]
}

case class NumberParameterRecord(ID: String, number: String) extends ParameterRecord
object NumberParameterRecord {
  implicit lazy val parser = Macro.namedParser[NumberParameterRecord]
}

case class ReferenceParameterRecord(ID: String) extends ParameterRecord
object ReferenceParameterRecord {
  implicit lazy val parser = Macro.namedParser[ReferenceParameterRecord]
}

sealed trait ParameterRecord {
  def ID: String
}

object ParameterRecord {
  implicit lazy val parser: RowParser[ParameterRecord] = SqlParser.str("class").flatMap {
    case "LAStringParameter" => implicitly[RowParser[StringParameterRecord]]
    case "LANumberParameter" => implicitly[RowParser[NumberParameterRecord]]
    case "LAReferenceParameter" => implicitly[RowParser[ReferenceParameterRecord]]
    case d => RowParser.failed[ParameterRecord](Error(SqlMappingError(
      "unexpected row type \'%s\'; expected: %s".format(d, "LAStringParameter, LANumberParameter"))
    ))
  }
}


case class CategoryRecord(ID: String, name: String)
object CategoryRecord {
  implicit lazy val parser = Macro.namedParser[CategoryRecord]
}

case class ScheduledTransactionRecord(ID: String, date: String, name: String)
object ScheduledTransactionRecord {
  implicit lazy val parser = Macro.namedParser[ScheduledTransactionRecord]
}

case class ScheduledTransactionSplitRecord(ID: String, transaction: String, category: String, amount: Option[String])
object ScheduledTransactionSplitRecord {
  implicit lazy val parser = Macro.namedParser[ScheduledTransactionSplitRecord]
}

case class ScheduledTransaction(name: String, amount: Option[Float], date: LocalDate)

object ScheduledTransaction {

  def apply(name: String, amount: Option[String], date: String): ScheduledTransaction = {
    val d = LocalDate.parse(date, DateTimeFormatter.ISO_LOCAL_DATE)
    ScheduledTransaction(name, amount.map(_.toFloat), d)
  }
}
