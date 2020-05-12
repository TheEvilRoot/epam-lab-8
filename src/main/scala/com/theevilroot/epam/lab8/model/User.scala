package com.theevilroot.epam.lab8.model
import com.theevilroot.epam.lab8.Statements.Basic
import com.theevilroot.epam.lab8.Statements.Basic._

class User (val id: Int,
            val name: String,
            val age: Int,
            val county: String) {


  case class UserEntry() extends Entry[User] {
    override def asRValue(name: String): Basic.RStatement = name match {
      case "id" => NumericStatement(id)
      case "name" => LiteralStatement(name)
      case "age" => NumericStatement(age)
      case "country" => LiteralStatement(county)
      case _ => throw new RuntimeException("Field " + name + " cannot be found in User")
    }

    override def canCompare[B <: Statement](statement: B, name: String): Boolean = name match {
      case "id" | "age" => statement match {
        case LiteralStatement(_) => true
        case _ => false
      }
      case "name" | "country" => statement match {
        case NumericStatement(_) => true
        case _ => false
      }
    }

    override def getDefaultValue(name: String): Option[RStatement] = name match {
      case "country" => Option.apply(asRValue(name))
      case _ => Option.empty
    }

    override def getFields: List[String] = List("id", "name", "age", "country")
  }
}
