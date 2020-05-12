package com.theevilroot.epam.lab8.model

import scala.collection.mutable

class DatabaseManager[A] {

  val databases: mutable.Map[String, Database[A]] = mutable.Map()
  var selectedDatabase: Database[A] = _

  def drop(databaseName: String): Boolean = databases.remove(databaseName) match {
    case None => false
    case Some(_) => true
  }

  def create(databaseName: String): Boolean = databases.put(databaseName, new Database[A](databaseName)) match {
    case None => true
    case Some(_) => false
  }

  def selectDatabase(databaseName: String): Boolean = databases.get(databaseName) match {
    case None => false
    case Some(_) => true
  }

}
