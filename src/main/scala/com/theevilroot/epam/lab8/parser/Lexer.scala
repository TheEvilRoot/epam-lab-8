package com.theevilroot.epam.lab8.parser

import com.theevilroot.epam.lab8.Statements.General._
import com.theevilroot.epam.lab8.Statements.Basic._
import com.theevilroot.epam.lab8.Statements.Construct._
import com.theevilroot.epam.lab8.Statements.Operator._

import scala.util.parsing.combinator.RegexParsers


class Lexer {

  val commandLexer = new CommandLexer

  class CommandLexer extends RegexParsers {

    def nameStatement: Parser[NameStatement] = "[a-zA-Z][a-zA-Z0-9]*".r - predicateWord - selectorWord ^^ { NameStatement }

    def number: Parser[NumericStatement] = "(0|[1-9]\\d*)".r ^^ { x => NumericStatement(x.toInt) }

    def whitespace: Parser[NullStatement] = "\\s+".r ^^ { _ => NullStatement() }

    def literal: Parser[LiteralStatement] = "['\"]".r ~ """([^'"]|(?<=\\)'")*""".r ~ "['\"]".r ^^ {
      case _ ~ value ~ _ => new LiteralStatement(value)
    }

    def rvalueStatement: Parser[RStatement] = number | literal | whitespace | nameStatement

    def multipleStatements[A <: Statement](f: => Parser[A], delim: => Parser[String]): Parser[StatementList[A]] =
      f ~ opt(delim ~ opt(multipleStatements(f, delim))) ^^ {
        case a ~ None => new StatementList[A](a)
        case a ~ Some(_ ~ None) => new StatementList[A](a)
        case a ~ Some(_ ~ Some(b)) => new StatementList[A](a, Option.apply(b))
      }

    def equalsStatement: Parser[EqualsStatement] = nameStatement ~ "=" ~ rvalueStatement ^^ {
      case name ~ _ ~ value =>
        EqualsStatement(name, value)
    }

    def predicateOperator: Parser[PredicateOperator] = ("==" | "!=") ^^ {
      case "==" => PredicateEquals()
      case "!=" => PredicateNotEquals()
      case a => throw new RuntimeException("Unknown predicate operator " + a)
    }

    def booleanOperator: Parser[BooleanOperator] = ("and" | "or" | "not") ^^ {
      case "and" => BooleanAnd()
      case "or" => BooleanOr()
      case "not" => BooleanNot()
      case a => throw new RuntimeException("Unknown boolean operator " + a)
    }

    def predicateStatement: Parser[PredicateStatement] = nameStatement ~ predicateOperator ~ rvalueStatement ^^ {
      case name ~ op ~ rvalue => new PredicateStatement(name, op, rvalue)
    }

    def booleanStatement: Parser[BooleanStatement] = predicateStatement ~ opt(booleanOperator) ~ opt(booleanStatement) ^^ {
      case pred ~ op ~ st => new BooleanStatement(pred, op, st)
    }

    def commandWord: Parser[String] = "create" | "select" | "drop" | "insert" | "update"
    def selectorWord: Parser[String] = "into" | "from"
    def predicateWord: Parser[String] = "where"
    def instructionDelimiter: Parser[String] = ";" | "\n"

    def command: Parser[Command] = commandWord ~ opt(multipleStatements[Statement](nameStatement | equalsStatement, "")) ^^ {
      case cmd ~ args => cmd match {
        case "create" => CreateCommand(args)
        case "select" => SelectCommand(args)
        case "drop" => DropCommand(args)
        case "insert" => InsertCommand(args)
        case "update" => UpdateCommand(args)
        case a => throw new RuntimeException("Unknown command " + a)
      }
    }

    def selector: Parser[Selector] = selectorWord ~ nameStatement ^^ {
      case name ~ stmt => name match {
        case "into" => IntoSelector(stmt)
        case "from" => FromSelector(stmt)
        case a => throw new RuntimeException("Unknown selector " + a)
      }
    }

    def predicate: Parser[Predicate] = predicateWord ~ booleanStatement ^^ {
      case name ~ stmt => name match {
        case "where" => WherePredicate(stmt)
        case a => throw new RuntimeException("Unknown predicate " + a)
      }
    }

    def instruction: Parser[InstructionStatement] =
      command ~ opt(selector) ~ opt(predicate) ^^ {
        case cmd ~ selector ~ pred => InstructionStatement(cmd, selector, pred)
      }

    def instructionSet: Parser[StatementList[InstructionStatement]] =
      multipleStatements[InstructionStatement](instruction, instructionDelimiter)
  }

  def tokenize(string: String): commandLexer.ParseResult[StatementList[InstructionStatement]] =
    commandLexer.parse(commandLexer.instructionSet, string)

  def extractInstructions(string: String): StatementList[InstructionStatement] = tokenize(string) match {
    case error: commandLexer.Failure => throw new RuntimeException(error.msg)
    case command => command.get
  }
}

