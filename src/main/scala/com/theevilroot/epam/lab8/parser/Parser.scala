package com.theevilroot.epam.lab8.parser

import com.theevilroot.epam.lab8.Statements.Basic._
import com.theevilroot.epam.lab8.Statements.Construct.{BooleanStatement, PredicateStatement}
import com.theevilroot.epam.lab8.Statements.General._
import com.theevilroot.epam.lab8.Statements.Operator.{BooleanAnd, BooleanNot, BooleanOperator, BooleanOr, PredicateEquals, PredicateNotEquals}
import com.theevilroot.epam.lab8.model.{Database, DatabaseManager, Entry}
import com.theevilroot.epam.lab8.parser.Commits._

class Parser {

  def booleanOperator(booleanOperator: BooleanOperator)(a: Boolean, b: Boolean): Boolean = booleanOperator match {
    case BooleanOr() => a || b
    case BooleanAnd() => a && b
    case BooleanNot() => !a
  }

  def parsePredicate[A](predicateStatement: PredicateStatement)(entry: Entry): Boolean =
    if (entry.canCompare(predicateStatement.value, predicateStatement.name.name)) {
      val compResult = entry.asRValue(predicateStatement.name.name).equals(predicateStatement.value)
      predicateStatement.predicateOperator match {
        case PredicateEquals() => compResult
        case PredicateNotEquals() => !compResult
      }
    } else throw new RuntimeException("Cannot compare predicate rvalue " + predicateStatement.value +
      " and " + entry.asRValue(predicateStatement.name.name))

  def parseBoolean[A](booleanStatement: BooleanStatement): Entry => Boolean = booleanStatement.operator match {
    case None => parsePredicate(booleanStatement.predicateStatement)
    case Some(op) => e =>
      booleanOperator(op)(parsePredicate(booleanStatement.predicateStatement)(e), booleanStatement.operand match {
        case Some(operand) => parseBoolean(operand)(e)
        case None if op == BooleanOr() => false
        case None => throw new RuntimeException("Not enough operand for operator " + op + " in statement " + booleanStatement)
      })
  }

  def parseSelector[A](selector: Selector)(dbm: DatabaseManager[A]): Database[A] =
    dbm.selectDatabase(selector.nameStatement.name) match {
      case true => dbm.selectedDatabase
      case false => throw new RuntimeException("Failed to select database by selector " + selector)
    }

  def parseInstructions(list: StatementList[InstructionStatement]): List[Commit] = list.next match {
    case Some(a) => parseInstructions(a).appended(parseInstruction(list.statement))
    case None => List(parseInstruction(list.statement))
  }

  def parseInstruction[A](ins: InstructionStatement):
  (DatabaseManager[A], DatabaseManager[A] => Database[A], Entry[A] => Boolean) => Boolean = ins match {
    case InstructionStatement(command, selector, pred) => command match {
      case DropCommand(Some(table)) => table.statement match {
        case NameStatement(name) => (dbm, _, _) => dbm.drop(name)
        case a => throw new RuntimeException("Invalid argument type for drop command " + a)
      }
      case CreateCommand(Some(table)) => table.statement match {
        case NameStatement(name) => (dbm, _, _) => dbm.create(name)
        case a => throw new RuntimeException("Invalid argument type for create command " + a)
      }
      case SelectCommand(Some(names)) => names.next match {
        case None => names.statement match {
          case NameStatement(name) => (dbm, _, _) => dbm.selectDatabase(name)
          case a => throw new RuntimeException("Invalid argument type for select database command " + a)
        }
        case Some(_) => (dbm, getDatabase, pred) => getDatabase(dbm).forEachPredicate(pred, x => println(x)) true
      }
      case InsertCommand(Some(pairs)) => (dbm, getDatabase, _) =>
        getDatabase(dbm).
      case UpdateCommand(Some(pairs)) => UpdateCommit(pairs, Option.apply(selector.name), pred)

      case a => throw new RuntimeException("Not enough information for command " + a.name)
    }
  }

}
