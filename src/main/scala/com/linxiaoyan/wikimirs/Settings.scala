package com.linxiaoyan.wikimirs

import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

object Settings {

  var config = ConfigFactory.load()

  def getString = config.getString _
  def getInt = config.getInt _
  def getDouble = config.getDouble _

  def set(config: Config) {
    this.config = config
  }

  def reset {
    ConfigFactory.invalidateCaches();
    config = ConfigFactory.load()
  }
}