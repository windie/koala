package com.linxiaoyan.wikimirs

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

case class LabelUnit(val query: String, val url: String) {
}

trait LabelStorager {

  def get(labelUnit: LabelUnit): Int

  def put(labelUnit: LabelUnit, relevance: Int): Unit

  def allQueries: Seq[String]
}

object LabelStorager {
  def apply(): LabelStorager = BDBLabelStorager
}

object BDBLabelStorager extends LabelStorager {

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
    envConfig.setReadOnly(false)

    val env = new Environment(file, envConfig)
    env.openDatabase(null, "label", createDatabaseConfig);
  }

  private def createDatabaseConfig: DatabaseConfig = {
    val dbConfig = new DatabaseConfig()
    dbConfig.setAllowCreate(true)
    dbConfig.setTransactional(true)
    dbConfig.setReadOnly(false)
    dbConfig
  }

  private def get(query: String): Option[JsArray] = {
    val key = new DatabaseEntry(query.getBytes(UTF8))
    val value = new DatabaseEntry()
    if (db.get(null, key, value, LockMode.DEFAULT) == OperationStatus.SUCCESS) {
      val json = new String(value.getData, UTF8)
      Some(Json.parse(json).asInstanceOf[JsArray])
    } else {
      None
    }
  }

  def get(labelUnit: LabelUnit): Int = {
    synchronized {
      get(labelUnit.query) match {
        case Some(array) => {
          for (o <- array.value) {
            val label = Json.fromJson(o).get
            if (label.url == labelUnit.url) {
              return label.relevance
            }
          }
          0
        }
        case None => 0
      }
    }
  }

  def put(labelUnit: LabelUnit, relevance: Int): Unit = {
    synchronized {
      val o = LabelValue(labelUnit.url, relevance)
      val r = get(labelUnit.query) match {
        case Some(array) => {
          val filtered = array.value.filter { x =>
            val label = Json.fromJson(x).get
            label.url != labelUnit.url
          }.toList
          if (relevance == 0) {
            filtered
          } else {
            filtered :+ Json.toJson(o)
          }
        }
        case None => {
          if (relevance == 0) {
            Nil
          } else {
            List(Json.toJson(o))
          }
        }
      }
      val key = new DatabaseEntry(labelUnit.query.getBytes(UTF8))
      if (r.isEmpty) {
        db.delete(null, key)
      } else {
        val value = new DatabaseEntry(Json.stringify(JsArray(r)).getBytes(UTF8))
        db.put(null, key, value)
      }
    }
  }

  def allQueries: Seq[String] = {
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

