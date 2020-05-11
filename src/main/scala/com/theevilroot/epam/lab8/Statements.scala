package com.theevilroot.epam.lab8

import com.theevilroot.epam.lab8.Statements.Basic._
import com.theevilroot.epam.lab8.Statements.Construct._
import com.theevilroot.epam.lab8.Statements.Operator._

object Statements {

  object Basic {
    class Statement(val statementValue: String) {
      override def toString: String =
        "statement[" + statementValue + "]"
    }

    class RStatement(value: String) extends Statement(value)

    class LiteralStatement(val value: String) extends RStatement(value) {
      override def toString: String =
        "literal[" + value + "]"
    }

    case class NumericStatement(val value: Int) extends RStatement(value.toString) {
      override def toString: String =
        "numeric[" + value.toString + "]"
    }

    case class NameStatement(val name: String) extends RStatement(name) {
      override def toString: String =
        "name[" + name + "]"
    }

    case class NullStatement() extends RStatement("null")

    case class EqualsStatement(val varName: NameStatement,
                               val value: RStatement) extends Statement(varName + "=" + value) {
      override def toString: String =
        "equals[" + varName + "=" + value + "]"
    }

    class StatementList[A <: Statement] (val statement: A,
                                         val next: Option[StatementList[A]] = Option.empty)
      extends Statement(this.toString) {
      override def toString: String = next match {
        case None => statement.toString
        case Some(a) => statement + ", " + a
      }
    }
  }

  object Construct {
    class PredicateStatement(val name: NameStatement,
                             val predicateOperator: PredicateOperator,
                             val value: RStatement) {
      override def toString: String =
        "predicate(" + name + " " + predicateOperator + " " + value + ")"
    }

    class BooleanStatement(val predicateStatement: PredicateStatement,
                           val operator: Option[BooleanOperator] = Option.empty,
                           val operand: Option[BooleanStatement] = Option.empty) {
      override def toString: String =
        "boolean(" + predicateStatement + ")("  + (operator match {
          case None => ")"
          case Some(a) => a + " -> " + (operand match {
            case None => "nothing)"
            case Some(b) => b + ")"
          })
        })
    }
  }

  object Operator {
    abstract class PredicateOperator(val name: String) {
      override def toString: String = name
    }
    case class PredicateEquals() extends PredicateOperator("equals")
    case class PredicateNotEquals() extends PredicateOperator("not equals")

    abstract class BooleanOperator(val name: String) {
      override def toString: String = name
    }
    case class BooleanAnd() extends BooleanOperator("and")
    case class BooleanOr() extends BooleanOperator("or")
    case class BooleanNot() extends BooleanOperator("not")
  }

  object General {
    abstract class Command(val name: String,
                           val args: Option[StatementList[Statement]] = Option.empty) {
      override def toString: String = args match {
        case None => name + "()"
        case Some(a) => name + "(" + a + ")"
      }
    }
    case class CreateCommand(override val args: Option[StatementList[Statement]]) extends Command("create", args)
    case class InsertCommand(override val args: Option[StatementList[Statement]]) extends Command("insert", args)
    case class UpdateCommand(override val args: Option[StatementList[Statement]]) extends Command("update", args)
    case class DropCommand(override val args: Option[StatementList[Statement]]) extends Command("drop", args)
    case class SelectCommand(override val args: Option[StatementList[Statement]]) extends Command("select", args)
    case class InvalidCommand(override val args: Option[StatementList[Statement]]) extends Command("invalid", args)

    abstract class Selector(val name: String,
                            val nameStatement: NameStatement) {
      override def toString: String =
        "selector(" + name + ")(" + nameStatement + ")"
    }
    case class FromSelector(fromName: NameStatement) extends Selector("from", fromName)
    case class IntoSelector(intoName: NameStatement) extends Selector("into", intoName)

    abstract class Predicate(val name: String,
                             val pred: BooleanStatement) {
      override def toString: String =
        "predicate(" + name + ")(" + pred + ")"
    }

    case class WherePredicate(override val pred: BooleanStatement)
      extends Predicate("where", pred)

    case class InstructionStatement(val command: Command,
                                    val selector: Option[Selector] = Option.empty,
                                    val predicate: Option[Predicate] = Option.empty) extends Statement(toString){
      override def toString: String = "" + command + " " + (selector match {
        case None => ""
        case Some(selector) => selector + " "
      }) + (predicate match {
        case None => ""
        case Some(predicate) => predicate + " "
      })
    }
  }












}
