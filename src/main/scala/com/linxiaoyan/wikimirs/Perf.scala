package com.linxiaoyan.wikimirs

import scala.io.Source
import scala.collection.mutable._
import java.net.URL
import java.net.URLEncoder
import org.apache.commons.io.IOUtils
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue

object Perf extends App {

  var minTime = Double.MaxValue
  var maxTime = 0.0
  var sumTime = 0.0
  var count = 0

  val times = ListBuffer[Double]()
  for (query <- PerfQuerySuite.allQueries) {
    println(query)
    val time = getQueryTime(query)
    minTime = minTime min time
    maxTime = maxTime max time
    sumTime += time
    count += 1
    times += time
  }
  val sortedTimes = times.toList.sortWith({
    case (a, b) => a < b
  })

  val meanTime = if (sortedTimes.size % 2 == 0) {
    (sortedTimes(sortedTimes.size / 2 - 1) + sortedTimes(sortedTimes.size / 2)) / 2
  } else {
    sortedTimes(sortedTimes.size / 2)
  }

  println("query: " + count)
  println("min: " + minTime)
  println("max: " + maxTime)
  println("mean:" + meanTime)
  println("avg: " + (sumTime / count))

  def getQueryTime(query: String): Double = {
    val conn = new URL("http://localhost:8080/api/search/" + URLEncoder.encode(query, "UTF-8") + "/1?size=10").openConnection()
    conn.connect()
    val input = conn.getInputStream()
    try {
      val json = IOUtils.toString(input)
      (Json.parse(json) \ "time").as[String].toDouble
    } finally {
      input.close()
    }
  }
}


import com.sleepycat.je.EnvironmentConfig
import java.io.File
import com.sleepycat.je.Environment
import com.sleepycat.je.DatabaseConfig
import com.sleepycat.je.Database
import com.sleepycat.je.DatabaseEntry
import com.sleepycat.je.LockMode
import com.sleepycat.je.OperationStatus
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer
import play.api.libs.json._
import play.api.libs.functional.syntax._

object PerfQuerySuite {

  case class LabelValue(url: String, relevance: Int)

  implicit val labelValueReads = Json.reads[LabelValue]
  implicit val labelValueWrites = Json.writes[LabelValue]

  private val UTF8 = Charset.forName("UTF-8")

  private val db = connect

  private def connect: Database = {
    val file = new File(Settings.getString("label.dir"))
    if (!file.exists()) {
      file.mkdirs()
    }

    val envConfig = new EnvironmentConfig
    envConfig.setAllowCreate(true)
    envConfig.setTransactional(true)
    envConfig.setReadOnly(true)

    val env = new Environment(file, envConfig)
    env.openDatabase(null, "label", createDatabaseConfig);
  }

  private def createDatabaseConfig: DatabaseConfig = {
    val dbConfig = new DatabaseConfig()
    dbConfig.setAllowCreate(true)
    dbConfig.setTransactional(true)
    dbConfig.setReadOnly(true)
    dbConfig
  }

  def allQueries: List[String] = {
    synchronized {
      val cursor = db.openCursor(null, null)
      try {
        val key = new DatabaseEntry
        val value = new DatabaseEntry
        val queries = ListBuffer[String]()
        while (cursor.getNext(key, value, LockMode.DEFAULT) == OperationStatus.SUCCESS) {
          queries += new String(key.getData(), UTF8)
        }
        queries.toList
      } finally {
        cursor.close()
      }
    }
  }
}