package com.theevilroot.epam.lab8.model

import scala.collection.mutable

class DatabaseManager {

  val databases: mutable.Map[String, Database] = mutable.Map()
  var selectedDatabase: Database = _

  def drop(tableName: String): Boolean = databases.contains(tableName) match {
    case false => false
    case true => databases.remove(tableName) true
  }

  def create(tableName: String): Boolean = databases.contains(tableName) match {
    case true => false
    case false => databases.addOne((tableName, new Database(tableName))) true
  }

}
