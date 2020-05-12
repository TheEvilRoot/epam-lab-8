package com.theevilroot.epam.lab8.model

import com.theevilroot.epam.lab8.Statements.Basic.RStatement

class Database[A](val name: String,
                  val entries: List[Entry[A]] = List()) {

  def applyByPredicate(pred: Entry[A] => Boolean, f: Entry[A] => Unit): Unit =
    entries.filter(pred).foreach(f)



}
