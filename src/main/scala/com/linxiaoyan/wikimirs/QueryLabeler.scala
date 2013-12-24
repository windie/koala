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

case class LabelUnit(val query: String, val formula: String, val url: String) {
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
            val label = o.asInstanceOf[JsObject]
            if ((label \ "formula").as[String] == labelUnit.formula &&
              (label \ "url").as[String] == labelUnit.url) {
              return (label \ "score").as[Int]
            }
          }
          -1
        }
        case None => -1
      }
    }
  }

  def put(labelUnit: LabelUnit, relevance: Int): Unit = {
    synchronized {
      val o = Json.obj(
        "formula" -> labelUnit.formula,
        "url" -> labelUnit.url,
        "score" -> relevance)
      val r = get(labelUnit.query) match {
        case Some(array) => {
          val filtered = array.value.filter { x =>
            val label = x.asInstanceOf[JsObject]
            !((label \ "formula").as[String] == labelUnit.formula &&
              (label \ "url").as[String] == labelUnit.url)
          }.toList
          if (relevance < 0) {
            filtered
          } else {
            filtered :+ o
          }
        }
        case None => {
          if (relevance < 0) {
            Nil
          } else {
            List(o)
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

