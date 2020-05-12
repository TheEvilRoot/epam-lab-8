package com.theevilroot.epam.lab8.model

import com.theevilroot.epam.lab8.Statements.Basic.{RStatement, Statement}

trait Entry[A] {

  def value: A

  def asRValue(name: String): RStatement

  def canCompare[B <: Statement](statement: B, name: String): Boolean

  def getDefaultValue(name: String): Option[RStatement]

}
