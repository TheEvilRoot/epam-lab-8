package com.theevilroot.epam.lab8

import scala.collection.mutable
import scala.io.StdIn
import scala.util.parsing.combinator.RegexParsers

object Main {

  abstract class Statement

  class RValueStatement(val value: String)
  case class NumericStatement(val intValue: Int) extends RValueStatement(intValue.toString)
  case class LiteralStatement(val stringValue: String) extends RValueStatement(stringValue)

  case class FieldStatement(val key: String, val value: RValueStatement) extends Statement

  case class CommandStatement(val command: String, val args: List[FieldStatement]) extends Statement

  abstract class Variable(val name: String)
  case class IntVar(override val name: String, val value: Int) extends Variable(name)
  case class StringVar(override val name: String, val value: String) extends Variable(name)

  object DatabaseParser extends RegexParsers {

    def commandKw: Parser[String] =
      "get" | "add" | "delete" | "update"

    def numeric: Parser[NumericStatement] =
     "(0|[1-9]\\d*)".r ^^ (a => NumericStatement(a.toInt))

    def literal: Parser[LiteralStatement] =
      "['\"]".r ~ """([^'"]|(?<=\\)'")*""".r ~ "['\"]".r ^^ { case _ ~ value ~ _ => LiteralStatement(value) }

    def rvalue: Parser[RValueStatement] =
      (numeric | literal)

    def name: Parser[String] =
      "[a-zA-Z][a-zA-Z0-9]*".r

    def field: Parser[FieldStatement] =
      name ~ "=" ~ rvalue ^^ {
        case key ~ _ ~ value => FieldStatement(key, value)
      }

    def args: Parser[List[FieldStatement]] =
      field ~ opt(args) ^^ {
        case a ~ None => List(a)
        case a ~ Some(rest) => a :: rest
      }

    def command: Parser[CommandStatement] =
      commandKw ~ opt(args) ^^ {
        case command ~ None => CommandStatement(command, List())
        case command ~ Some(args) => CommandStatement(command, args)
      }
  }

  def parse(str: String): CommandStatement = DatabaseParser.parse(DatabaseParser.command, str) match {
    case _: DatabaseParser.Failure => throw new RuntimeException("Failed to parse")
    case result => result.get
  }

  def evaluateArgs(args: List[FieldStatement]): Map[String, Variable] = args match {
    case a :: rest => evaluateArgs(rest) + (
      (a.key, a.value match {
        case NumericStatement(intValue) => IntVar(a.key, intValue)
        case LiteralStatement(stringValue) => StringVar(a.key, stringValue)
      })
    )
    case Nil => Map()
  }

  class User (val id: Int,
              val age: Int,
              val name: String,
              val country: String) {
    override def toString: String =
      "id: " + id + " age:" + age + " name:" + name + " country:" + country
  }

  def makeUser(args: Map[String, Variable]): User =
    new User(args.get("id") match {
      case None => throw new RuntimeException("user: no id passed")
      case Some(IntVar(_, id)) => id
      case Some(StringVar(_,_)) => throw new RuntimeException("user: id expected to be int, but actually string has passed")
    }, args.get("age") match {
      case None => throw new RuntimeException("user: no age passed")
      case Some(IntVar(_, age)) => age
      case Some(StringVar(_,_)) => throw new RuntimeException("user: age expected to be int, but actually string has passed")
    }, args.get("name") match {
      case None => throw new RuntimeException("user: no name passed")
      case Some(StringVar(_, name)) => name
      case Some(IntVar(_, value)) =>
        println("[Warning] Implicit conversion from int to string in name field")
        value.toString
    }, args.get("country") match {
      case None => throw new RuntimeException("user: no country passed")
      case Some(StringVar(_, country)) => country
      case Some(IntVar(_, value)) =>
        println("[Warning] Implicit conversion from int to string in country field")
        value.toString
    })

  def predicate(args: Map[String, Variable])(user: User): Boolean = (args.get("id") match {
    case Some(IntVar(_, id)) => user.id == id
    case Some(StringVar(_,_)) =>
      throw new RuntimeException("select: id expected to be int, but actually string has passed")
    case None => true
  }) && (args.get("age") match {
    case Some(IntVar(_, age)) =>
      println("[Warning] Implicit conversion from int to string in name field")
      user.age == age
    case Some(StringVar(_,_)) =>
      throw new RuntimeException("select: age expected to be int, but actually string has passed")
    case None => true
  }) && (args.get("name") match {
    case Some(IntVar(_, nameInt)) => user.name == nameInt.toString
    case Some(StringVar(_,name)) => user.name == name
    case None => true
  }) && (args.get("country") match {
    case Some(IntVar(_, countryInt)) => user.country == countryInt.toString
    case Some(StringVar(_,country)) => user.country == country
    case None => true
  })

  def select(in: List[User], args: Map[String, Variable]) =
    in.filter(predicate(args)).foreach(println(_))

  def delete(in: List[User], args: Map[String, Variable]): List[User] =
    in.filter(predicate(args))

  def update(in: List[User], args: Map[String, Variable]): List[User] = args.get("id") match {
    case None => throw new RuntimeException("update: no id passed to identify update user")
    case Some(StringVar(_, _)) =>
      throw new RuntimeException("update: id expected to be int, but actually string has passed")
    case Some(IntVar(_, id)) => in.find(x => x.id == id) match {
      case None => throw new RuntimeException("update: no user with id " + id)
      case Some(user) => in.dropWhile(x => x.id == id).appended(makeUser(args.withDefault {
        case "id" => IntVar("id", user.id)
        case "age" => IntVar("age", user.age)
        case "country" => StringVar("country", user.country)
        case "name" => StringVar("name", user.name)
      }))
    }
  }

  val users = new mutable.ListBuffer[User]

  def handler(cmd: String)(args: Map[String, Variable]):Unit = cmd match {
    case "add" => users.addOne(makeUser(args))
    case "get" => select(users.toList, args)
    case "delete" => users.clear(); users.addAll(delete(users.toList, args))
    case "update" => users.clear(); users.addAll(update(users.toList, args))
  }

  def evalCommand(stmt: CommandStatement): Unit =
    handler(stmt.command)(evaluateArgs(stmt.args))

  def main(args: Array[String]): Unit = {
      while (true) try {
        evalCommand(parse(StdIn.readLine("> ")))
      } catch {
        case _: NullPointerException => System.exit(0)
        case e: RuntimeException => println(e.getLocalizedMessage)
      }
    }

}
