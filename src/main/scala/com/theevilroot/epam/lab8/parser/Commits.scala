package com.theevilroot.epam.lab8.parser

import com.theevilroot.epam.lab8.Statements.Basic.{EqualsStatement, NameStatement, RStatement, Statement, StatementList}
import com.theevilroot.epam.lab8.Statements.General._
import com.theevilroot.epam.lab8.parser.Commits.PairsCommit

object Commits {

  abstract class Commit(val predicate: Option[Predicate] = Option.empty)

  abstract class FieldsCommit(val fieldsStatement: StatementList[Statement],
                              override val predicate: Option[Predicate] = Option.empty) extends Commit(predicate) {
    def getFields(list: StatementList[Statement])(): List[String] = list.statement match {
      case NameStatement(name) => list.next match {
        case None => List(name)
        case Some(a) => getFields(a).appended(name)
      }
      case a => throw new RuntimeException("Invalid type for this command " + a)
    }

    def fields: List[String] = getFields(fieldsStatement)
  }

  abstract class PairsCommit(val fieldsStatement: StatementList[Statement],
                             override val predicate: Option[Predicate] = Option.empty) extends Commit {
    def getPairs(list: StatementList[Statement])(): Map[String, RStatement] = list.statement match {
      case EqualsStatement(key, value) => list.next match {
        case None => Map[String, RStatement]().+((key.name, value))
        case Some(a) => getPairs(a).+((key.name, value))
      }
      case a => throw new RuntimeException("Invalid type for this command " + a)
    }

    def pairs: Map[String, Any] = getPairs(fieldsStatement)
  }

  case class DropCommit(val tableName: String,
                        override val predicate: Option[Predicate] = Option.empty)
    extends Commit

  case class CreateCommit(val tableName: String,
                          override val predicate: Option[Predicate] = Option.empty)
    extends Commit

  case class SelectDatabaseCommit(val dbName: String,
                                  override val predicate: Option[Predicate] = Option.empty)
    extends Commit

  case class SelectCommit(override val fieldsStatement: StatementList[Statement],
                          val selectedDatabase: Option[String] = Option.empty,
                          override val predicate: Option[Predicate] = Option.empty)
    extends FieldsCommit(fieldsStatement, predicate)

  case class InsertCommit(override val fieldsStatement: StatementList[Statement],
                     val selectedDatabase: Option[String] = Option.empty,
                     override val predicate: Option[Predicate] = Option.empty)
    extends PairsCommit(fieldsStatement, predicate)

  case class UpdateCommit(override val fieldsStatement: StatementList[Statement],
                          val selectedDatabase: Option[String] = Option.empty,
                          override val predicate: Option[Predicate] = Option.empty)
    extends PairsCommit(fieldsStatement, predicate)

}
