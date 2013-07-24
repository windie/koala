package com.linxiaoyan.wikimirs

import com.typesafe.config.ConfigFactory

object Settings {

  val config = ConfigFactory.load()
  
  def getString = config.getString _
  def getInt = config.getInt _
  def getDouble = config.getDouble _
}