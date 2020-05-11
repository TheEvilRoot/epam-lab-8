package com.theevilroot.epam.lab8

import com.theevilroot.epam.lab8.parser.{Lexer, Parser}

object Main {
  /**
   * База данных пользователей. Необходимо реализовать:
   * добавление нового пользователя,
   * задав имя, возраст и страну;
   * изменение возраста или страны;
   * вывод информации о пользователе.
   */

  def main(args: Array[String]): Unit = {
    val lexer = new Lexer
    val parser = new Parser
    val stmt = lexer.extractInstructions("select everything from users where a==1 or b == 'hello world'")
    val commit = parser.parseInstructions(stmt)
    println(commit)
  }
}
