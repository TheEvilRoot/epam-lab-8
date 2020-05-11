package com.theevilroot.epam.lab8.parser

import com.theevilroot.epam.lab8.Statements.Basic.{EqualsStatement, NameStatement, Statement, StatementList}
import com.theevilroot.epam.lab8.Statements.General._

object Commits {

  class Commit(val predicate: Option[Predicate] = Option.empty)

  case class DropCommit(val tableName: String, override val predicate: Option[Predicate] = Option.empty) extends Commit
  case class CreateCommit(val tableName: String, override val predicate: Option[Predicate] = Option.empty) extends Commit
  case class SelectDatabaseCommit(val dbName: String, override val predicate: Option[Predicate] = Option.empty) extends Commit
  case class SelectCommit(val fieldsStatement: StatementList[Statement],
                          val selectedDatabase: Option[String] = Option.empty,
                          override val predicate: Option[Predicate] = Option.empty) extends Commit{
    def getFields(list: StatementList[Statement])(): List[String] = list.statement match {
      case NameStatement(name) => list.next match {
        case None => List(name)
        case Some(a) => getFields(a).appended(name)
      }
      case a => throw new RuntimeException("Invalid type for select command " + a)
    }

    def fields: List[String] = getFields(fieldsStatement)
  }

  class InsertCommit(val fieldsStatement: StatementList[Statement],
                          val selectedDatabase: Option[String] = Option.empty,
                          override val predicate: Option[Predicate] = Option.empty) extends Commit{
    def getPairs(list: StatementList[Statement])(): Map[String, Any] = list.statement match {
      case EqualsStatement(key, value) => list.next match {
        case None => Map().+((key.name, value.statementValue))
        case Some(a) => getPairs(a).+((key.name, key.statementValue))
      }
      case a => throw new RuntimeException("Invalid type for select command " + a)
    }

    def pairs: Map[String, Any] = getPairs(fieldsStatement)
  }

  case class UpdateCommit(override val fieldsStatement: StatementList[Statement],
                          override val selectedDatabase: Option[String] = Option.empty,
                          override val predicate: Option[Predicate] = Option.empty)
    extends InsertCommit(fieldsStatement, selectedDatabase, predicate)

}
