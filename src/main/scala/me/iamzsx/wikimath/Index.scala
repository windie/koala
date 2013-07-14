package me.iamzsx.wikimath

import java.io.File
import java.net.URLEncoder
import java.util.concurrent.atomic.AtomicLong

import scala.actors.Actor
import scala.xml._

import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document._
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

import com.typesafe.config.ConfigFactory

case class Finish()
case class Stop()

trait FileScanner extends Actor

class FileScannerImpl(root: File, parallel: Int, indexer: Indexer) extends FileScanner {

  val workers: Seq[ParseWorker] = (0 until parallel) map { x =>
    val worker = new ParseWorker(this, indexer)
    worker.start
    this ! worker
    worker
  }

  def scan(file: File) {
    if (file.isDirectory()) {
      for (subFile <- file.listFiles()) {
        scan(subFile)
      }
    } else {
      sendFile(file)
    }
  }

  private def sendFile(file: File) {
    receive {
      case worker: ParseWorker => {
        worker ! file
      }
    }
  }

  private def noticeStop() {
    workers foreach (_ ! Stop)
    exit
  }

  def act() {
    scan(root)
    noticeStop
  }

}

class ParseWorker(val scanner: FileScanner, val indexer: Indexer) extends Actor {

  def act() {
    loop {
      react {
        case file: File => {

          val xml = XML.loadFile(file)
          for (pageNode <- xml \\ "page") {
            val page = new WikiPage(pageNode)
            for (math <- page.mathes) {
              indexer ! (math, page)
            }
          }
          scanner ! this
        }
        case Stop => {
          indexer ! Stop
          exit
        }
      }
    }
  }
}

object ParseWorker {
  val MATH = """(?s)\<math\>(.*?)\</math\>""".r
}

class FormulaDocument(val latex: String, val page: WikiPage) {

  val id = WikiPage.ID.incrementAndGet()

  def toDocument: Document = {
    val doc = new Document;

    doc.add(new StoredField("formula_id", id))
    doc.add(new TextField("formula", "test a b test a a", Field.Store.YES))
    doc.add(new StoredField("doc_id", page.id))
    doc.add(new StoredField("doc_title", page.title))
    doc.add(new StoredField("doc_title", page.title))

    doc
  }
}

object FormulaDocument {

  val ID = new AtomicLong(0)
}

class WikiPage(pageNode: Node) {

  val titleNodes = (pageNode \\ "title")

  assert(titleNodes.length == 1)

  val title = titleNodes.text

  val content = (pageNode \\ "text")

  assert(content.length == 1)

  val mathes = for (ParseWorker.MATH(math) <- ParseWorker.MATH findAllIn content.text) yield math

  val id = WikiPage.ID.incrementAndGet()

  val url = "http://en.wikipedia.org/wiki/" + URLEncoder.encode(title, "UTF-8")
}

object WikiPage {
  val ID = new AtomicLong(0)
}

trait Indexer extends Actor

class IndexerImpl(indexPath: File, parallel: Int) extends Indexer {

  val writer = {
    val dir = FSDirectory.open(indexPath)
    val analyzer = new FormulaAnalyzer
    val iwc = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    iwc.setOpenMode(OpenMode.CREATE)
    // iwc.setRAMBufferSizeMB(256.0);
    new IndexWriter(dir, iwc)
  }

  var remainWorker = parallel

  def act {
    loop {
      react {
        case (latex: String, page: WikiPage) =>
          val formula = new FormulaDocument(latex, page)
          writer.addDocument(formula.toDocument)
        case Stop => {
          remainWorker -= 1
          if (remainWorker == 0) close
        }
      }
    }
  }

  def close {
    writer.forceMerge(1)
    writer.close()
    exit
  }

}

object Config {
  val config = ConfigFactory.load()
  def get = config
}

object IndexApp {

  def main(args: Array[String]) {
    val parallel = Config.get.getInt("index.parallel")

    val indexer = new IndexerImpl(new File(Config.get.getString("index.dir")), parallel)
    indexer.start

    val scanner = new FileScannerImpl(new File(Config.get.getString("datasource.dir")), parallel, indexer)
    scanner.start
  }
}
