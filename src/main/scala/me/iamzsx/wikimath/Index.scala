package me.iamzsx.wikimath

import java.io.File
import scala.actors.Actor
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version
import org.apache.lucene.document.Document
import scala.xml.XML
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
          val content = (xml \\ "text").text
          for (ParseWorker.MATH(math) <- ParseWorker.MATH findAllIn content) {
            indexer ! math
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

class FormulaDocument(latex: String) {

  def toDocument: Document = {
    val doc = new Document;
    // TODO
    doc
  }
}

trait Indexer extends Actor

class IndexerImpl(indexPath: File, parallel: Int) extends Indexer {

  val writer = {
    val dir = FSDirectory.open(indexPath)
    val analyzer = new StandardAnalyzer(Version.LUCENE_43)
    val iwc = new IndexWriterConfig(Version.LUCENE_43, analyzer)
    iwc.setOpenMode(OpenMode.CREATE)
    // iwc.setRAMBufferSizeMB(256.0);
    new IndexWriter(dir, iwc)
  }

  var remainWorker = parallel

  def act {
    loop {
      react {
        case latex: String =>
          val formula = new FormulaDocument(latex)
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

object Index {

  def main(args: Array[String]) {
    val parallel = 4

    val indexer = new IndexerImpl(new File(Config.get.getString("index.dir")), parallel)
    indexer.start

    val scanner = new FileScannerImpl(new File(Config.get.getString("datasource.dir")), parallel, indexer)
    scanner.start
  }
}