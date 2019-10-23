package sre.api.finance
package icompta

import cm.CMStatement

sealed trait Rule {
  def name: String
  def test(transaction: CMStatement): Boolean
}

case class RulesGroup(id: String, name: String, rules: List[Rule]) extends Rule {

  def test(transaction: CMStatement): Boolean = {

    @annotation.tailrec
    def step(rules: Seq[Rule]): Boolean =
      rules match {
        case rule :: _ if rule.test(transaction) =>
          true

        case _ :: tail =>
          step(tail)

        case Nil => false
      }

    step(rules)
  }
}

case class SingleRule(id: String, name: String, condition: Condition) extends Rule {

  def test(transaction: CMStatement): Boolean =
    condition.test(transaction)
}

sealed trait ComparisonOp {
  def value: String
}

object ComparisonOp {

  def apply(s: String): ComparisonOp =
    s match {
      case s if s == Contains.value => Contains
      case s if s == BeginWith.value => BeginWith
      case s if s == Equals.value => Equals
      case s if s == LowerThan.value => LowerThan
      case s if s == GreaterThan.value => GreaterThan
      case s if s == IsDescendantOfCategory.value => IsDescendantOfCategory
      case x => sys.error(s"Unable to create ComparisonOp for $s")
    }

  case object Contains extends ComparisonOp {
    def value = "CONTAINS[cd]"
  }
  case object BeginWith extends ComparisonOp {
    def value = "BEGINSWITH[cd]"
 } 
  case object Equals extends ComparisonOp {
    def value = "="
  }
  case object LowerThan extends ComparisonOp {
    def value = "<"
  }
  case object GreaterThan extends ComparisonOp {
    def value = ">"
  }
  case object IsDescendantOfCategory extends ComparisonOp {
    def value = "isDescendantOfCategory:"
  }
}

sealed trait CompoundType {
  def label: String
  def value: Int
}

object CompoundType {

  def apply(i: Int): CompoundType =
    i match {
      case n if n == None.value => None
      case n if n == All.value => All
      case n if n == Any.value => Any
      case x => sys.error(s"Unable to create CompoundType for $i")
    }

  case object None extends CompoundType {
    def label = "None"
    def value = 0
  }

  case object All extends CompoundType {
    def label = "All"
    def value = 1
  }

  case object Any extends CompoundType {
    def label = "Any"
    def value = 2
  }
}

sealed trait Condition {

  def test(transaction: CMStatement): Boolean
}

case class CompoundCondition(
  id: String,
  `type`: CompoundType,
  conditions: List[Condition]
) extends Condition {

  def test(transaction: CMStatement): Boolean = {
    `type` match {
      case CompoundType.None =>
        conditions.forall { condition =>
          !condition.test(transaction)
        }

      case CompoundType.All =>
        conditions.forall { condition =>
          condition.test(transaction)
        }

      case CompoundType.Any =>
        conditions.exists { condition =>
          condition.test(transaction)
        }
    }
  }
}

case class ComparisonCondition(
  id: String,
  op: ComparisonOp,
  parameter: Parameter
) extends Condition {

  def test(transaction: CMStatement): Boolean = {
    parameter match {
      case StringParameter(_, value) =>
        op match {
          case ComparisonOp.Contains =>
            transaction.label.contains(value)

          case ComparisonOp.BeginWith =>
            transaction.label.startsWith(value)

          case ComparisonOp.Equals =>
            transaction.label == value

          case x => sys.error(s"Unable to apply comparison $x for transaction $transaction ")
        }

      case NumberParameter(_, value) =>
        op match {
          case ComparisonOp.Equals =>
            transaction.amount == value.toFloat

          case ComparisonOp.LowerThan =>
            transaction.amount < value.toFloat

          case ComparisonOp.GreaterThan =>
            transaction.amount > value.toFloat

          case x => sys.error(s"Unable to apply comparison $x for $transaction")
        }

      case _ => false
    }
  }
}

sealed trait Parameter
case class StringParameter(id: String, value: String) extends Parameter
case class NumberParameter(id: String, value: Int) extends Parameter
case class ReferenceParameter(id: String) extends Parameter

case class RuleAst(rules: List[Rule]) {

  def traverse(path: List[String]): Option[Rule] = {

    @annotation.tailrec
    def step(path: List[String], rules: List[Rule], cursor: Option[Rule]): Option[Rule] = {
      path match {
        case headPath :: Nil =>
          rules.find(_.name == headPath)

        case headPath :: tailPath =>
          rules.find(_.name == headPath) match {
            case Some(rule: RulesGroup) =>
              step(tailPath, rule.rules, cursor = Some(rule))

            case _ => None
          }

        case Nil => None
      }
    }

    step(path, rules, cursor = None)
  }
}

object RulesAst {

  private def buildConditions(records: Records, conditionRecord: ConditionRecord): Condition = {
    conditionRecord match {
      case c: CompoundConditionRecord =>
        val typ = CompoundType(c.`type`)
        val childrenRecords = records.conditions.filter(_.parent.exists(parentId => parentId == c.ID))
        val children = childrenRecords.map(buildConditions(records, _))
        CompoundCondition(c.ID, typ, children)

      case c: ComparisonConditionRecord =>
        val op = ComparisonOp(c.operator)
        val parameter = records.parameterOrFail(c.parameter) match {
          case StringParameterRecord(id, string) =>
            StringParameter(id, string)

          case NumberParameterRecord(id, number) =>
            NumberParameter(id, number.toInt)

          case ReferenceParameterRecord(id) =>
            ReferenceParameter(id)
        }

        ComparisonCondition(c.ID, op, parameter)
    }
  }

  def build(records: Records, ruleRecord: RuleRecord): Rule = {
    ruleRecord match {
      case RulesGroupRecord(id, name, active, _) if active == 1 =>
        val children = records.rules.filter(_.parent == id).map(build(records, _))
        RulesGroup(id, name, children)

      case SingleRuleRecord(id, name, active, parent, conditionId) if active == 1 =>
        val conditionRecord = records.conditionOrFail(conditionId)
        val condition = buildConditions(records, conditionRecord)
        SingleRule(id, name, condition)
    }
  }

  def build(records: Records): RuleAst = {
    val heads = records.rules.filterNot { rule =>
      records.rules.exists(_.ID == rule.parent)
    }
    RuleAst(heads.map(build(records, _)))
  }
}
