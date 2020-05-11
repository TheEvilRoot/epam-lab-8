package com.theevilroot.epam.lab8.parser

import com.theevilroot.epam.lab8.Statements.Basic._
import com.theevilroot.epam.lab8.Statements.General._
import com.theevilroot.epam.lab8.parser.Commits._

class Parser {

  def parseInstructions(list: StatementList[InstructionStatement]): List[Commit] = list.next match {
    case Some(a) => parseInstructions(a).appended(parseInstruction(list.statement))
    case None => List(parseInstruction(list.statement))
  }

  def parseInstruction(ins: InstructionStatement): Commit = ins match {
    case InstructionStatement(command, None, None) => command match {
      case DropCommand(Some(table)) => table.statement match {
        case NameStatement(name) => DropCommit(name)
        case a => throw new RuntimeException("Invalid argument type for drop command " + a)
      }
      case CreateCommand(Some(table)) => table.statement match {
        case NameStatement(name) => CreateCommit(name)
        case a => throw new RuntimeException("Invalid argument type for create command " + a)
      }
      case SelectCommand(Some(names)) => names.next match {
        case None => names.statement match {
          case NameStatement(name) => SelectDatabaseCommit(name)
          case a => throw new RuntimeException("Invalid argument type for select database command " + a)
        }
        case Some(_) => SelectCommit(names)
      }
      case a => throw new RuntimeException("Not enough information for command " + a.name)
    }
    case InstructionStatement(command, Some(selector), pred) => command match {
      case SelectCommand(Some(fields)) => SelectCommit(fields, Option.apply(selector.name), pred)
      case InsertCommand(Some(pairs)) => new InsertCommit(pairs, Option.apply(selector.name), pred)
      case UpdateCommand(Some(pairs)) => UpdateCommit(pairs, Option.apply(selector.name), pred)
      case _ => throw new RuntimeException("Not enough information for command " + command.name)
    }
  }

}
