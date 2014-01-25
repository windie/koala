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
  for (query <- Source.fromFile(Settings.getString("perf.query_file")).getLines.filterNot(_.isEmpty)) {
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