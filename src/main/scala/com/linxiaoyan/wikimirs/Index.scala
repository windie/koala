package com.linxiaoyan.wikimirs

import java.io.File
import java.io.FileInputStream
import java.util.concurrent.atomic.AtomicLong

import scala.actors.Actor

import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StoredField
import org.apache.lucene.document.TextField
import org.apache.lucene.store.FSDirectory

import com.typesafe.config.ConfigFactory

import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants

case class Finish()
case class Stop()

class WikiXMLScanner(writer: FormulaIndexWriter, xmlFile: File, parallel: Int = 4) extends Actor {

  val xmlInputFactory = XMLInputFactory.newInstance()
  xmlInputFactory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false);
  xmlInputFactory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);

  val workers: Seq[IndexWorker] = (0 until parallel) map { x =>
    val worker = new IndexWorker(writer, this)
    worker.start
    this ! worker
    worker
  }

  object Status extends Enumeration {
    val TEXT, TITLE, PAGE, OTHER = Value
  }

  private def scan(file: File) {
    val streamReader = xmlInputFactory.createXMLStreamReader(new FileInputStream(file))
    var title: StringBuilder = new StringBuilder
    var text: StringBuilder = new StringBuilder
    var status = Status.OTHER
    var pageCount = 0
    var begin = System.currentTimeMillis()
    while (streamReader.hasNext()) {
      streamReader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          streamReader.getLocalName() match {
            case "page" =>
              status = Status.PAGE
            case "text" =>
              text.clear
              status = Status.TEXT
            case "title" =>
              title.clear
              status = Status.TITLE
            case _ =>
          }
        case XMLStreamConstants.CHARACTERS => {
          status match {
            case Status.TEXT => {
              text.append(streamReader.getText())
            }
            case Status.TITLE => {
              title.append(streamReader.getText())
            }
            case _ =>
          }
        }
        case XMLStreamConstants.ENTITY_REFERENCE =>
          {
            println("entity: " + streamReader.getLocalName())
          }
        case XMLStreamConstants.END_ELEMENT =>
          streamReader.getLocalName() match {
            case "page" =>
              val page = new WikiPage(title.toString, text.toString)
              sendPage(page)
              status = Status.OTHER
              pageCount += 1
              if (pageCount % 10000 == 0) {
                val end = System.currentTimeMillis()
                println("time: " + ((end - begin) / 1000))
                println(pageCount)
                begin = end;
              }
            case "text" =>
              status = Status.PAGE
            case "title" =>
              status = Status.PAGE
            case _ =>
          }
        case _ =>
      }
    }
  }

  private def sendPage(page: WikiPage) {
    receive {
      case worker: IndexWorker => {
        worker ! page
      }
    }
  }

  private def noticeStop() {
    workers foreach (_ ! Stop)
    var remainWorkers = workers.length
    while (remainWorkers > 0) {
      receive {
        case Finish => {
          remainWorkers -= 1
        }
      }
    }
    writer.close
    exit
  }

  override def act() {
    scan(xmlFile)
    noticeStop
  }
}

class IndexWorker(val writer: FormulaIndexWriter, val scanner: WikiXMLScanner) extends Actor {
  def act() {
    loop {
      react {
        case page: WikiPage => {
          for (math <- page.mathes) {
            val formula = new FormulaDocument(math, page)
            writer.add(formula)
          }
          scanner ! this
        }
        case Stop => {
          scanner ! Finish
          exit
        }
      }
    }
  }
}

class FormulaDocument(val latex: String, val page: WikiPage) {

  val id = WikiPage.ID.incrementAndGet()

  def toDocument: Document = {
    val doc = new Document;
    doc.add(new StoredField("formula_id", id))
    doc.add(new TextField("formula", latex, Field.Store.YES))
    doc.add(new StoredField("doc_id", page.id))
    doc.add(new StoredField("doc_title", page.title))

    doc
  }
}

object FormulaDocument {
  val ID = new AtomicLong(0)
}

case class WikiPage(val title: String, text: String) {
  val mathes = for (WikiPage.MATH(math) <- WikiPage.MATH findAllIn text) yield math
  val id = WikiPage.ID.incrementAndGet()
}

object WikiPage {
  val ID = new AtomicLong(0)
  val MATH = """(?s)\<math\>(.*?)\</math\>""".r
}

object IndexApp {

  def main(args: Array[String]) {
    val parallel = Settings.getInt("index.parallel")
    val dir = FSDirectory.open(new File(Settings.getString("index.dir")))
    val writer = new FormulaIndexWriter(dir)
    val data = new File(Settings.getString("index.wikipedia_export_file"));
    val scanner = new WikiXMLScanner(writer, data, parallel)
    scanner.start
  }
}
