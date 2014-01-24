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

/**
 * Scan the Wikipedia export file. {@link WikiXMLScanner} parses the file
 *  and send every {@link WikiPage} to one of {@link IndexWorker}.
 *
 * @see <a href="http://en.wikipedia.org/wiki/Help:Export">Help:Export</a>
 */
class WikiXMLScanner(
  formulaWriter: FormulaIndexWriter,
  pageWriter: PageIndexWriter,
  xmlFile: File,
  parallel: Int = 4) extends Actor {

  private val xmlInputFactory = XMLInputFactory.newInstance()
  xmlInputFactory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false);
  xmlInputFactory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);

  private val workers: Seq[IndexWorker] = (0 until parallel) map { x =>
    val worker = new IndexWorker(formulaWriter, pageWriter, this)
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
    formulaWriter.close
    pageWriter.close
    println("Pages: " + WikiPage.ID)
    println("Formulas: " + FormulaDocument.ID)
    exit
  }

  override def act() {
    scan(xmlFile)
    noticeStop
  }
}

class IndexWorker(
  formulaWriter: FormulaIndexWriter,
  pageWriter: PageIndexWriter,
  scanner: WikiXMLScanner) extends Actor {

  def act() {
    loop {
      react {
        case page: WikiPage => {
          formulaWriter.add(page)
          pageWriter.add(page)
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

class FormulaDocument(latex: String, page: WikiPage) {

  val id = FormulaDocument.ID.incrementAndGet()

  def toDocument: Document = {
    val doc = new Document;
    doc.add(new StoredField("formula_id", id))
    doc.add(new TextField("formula", latex, Field.Store.YES))
    doc.add(new StoredField("doc_id", page.id))
    doc.add(new StoredField("doc_title", page.title))
    doc
  }
}

class PageDocument(page: WikiPage) {

  def toDocument: Document = {
    val doc = new Document;
    val content = page.mathes.mkString("\0")
    doc.add(new TextField("formula", content, Field.Store.NO))
    doc.add(new StoredField("doc_id", page.id))
    doc.add(new StoredField("doc_title", page.title))
    doc
  }
}

object FormulaDocument {
  val ID = new AtomicLong(0)
}

case class WikiPage(val title: String, text: String) {
  val mathes = (for (WikiPage.MATH(math) <- WikiPage.MATH findAllIn text) yield math).toList
  val id = WikiPage.ID.incrementAndGet()
}

object WikiPage {
  val ID = new AtomicLong(0)
  val MATH = """(?s)\<math\>(.*?)\</math\>""".r
}

object IndexApp {

  private def getFSDirectory(dirConfigName: String): FSDirectory = {
    val dir = new File(Settings.getString(dirConfigName))
    if (dir.exists) {
      throw new IllegalStateException(dir + " exists. Delete it if you want to rebuild the index")
    }
    FSDirectory.open(dir)
  }

  def main(args: Array[String]) {
    val parallel = Settings.getInt("index.parallel")
    val formulaWriter = new FormulaIndexWriter(getFSDirectory("index.formula_dir"))
    val pageWriter = new PageIndexWriter(getFSDirectory("index.page_dir"))
    val data = new File(Settings.getString("index.wikipedia_export_file"));
    val scanner = new WikiXMLScanner(formulaWriter, pageWriter, data, parallel)
    scanner.start
  }
}
