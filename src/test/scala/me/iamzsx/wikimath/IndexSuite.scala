package me.iamzsx.wikimath

import scala.math._
import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import scala.io.Source
import java.io._

@RunWith(classOf[JUnitRunner])
class ParseWorkerSuite extends FunSuite {

  class FileScannerImplTest(indexer: Indexer) extends FileScanner {

    val worker: ParseWorker = {
      val worker = new ParseWorker(this, indexer)
      worker.start
      this ! worker
      worker
    }

    def act() {
      receive {
        case worker: ParseWorker =>
          worker ! new File(getClass.getResource("/Singular_value.xml").toURI())
      }
      worker ! Stop
      exit
    }

  }

  class IndexerImplTest extends Indexer {

    var latexes = List[String]()
    var isStop = false

    def act() {
      loop {
        react {
          case latex: String => {
            latexes = latexes ::: List(latex)
          }
          case Stop => {
            isStop = true
            exit
          }
        }
      }
    }

  }

  test("extract math latex string from xml") {
    val indexer = new IndexerImplTest
    indexer.start
    val scanner = new FileScannerImplTest(indexer)
    scanner.start
    Thread.sleep(1000)

    val expected = List(
      """\sqrt{A^*A}=U|\Lambda|U^*""",
      """s_n(T) = \inf\big\{\, \|T-L\| : L\text{ is an operator of finite rank }<n \,\big\}.""")

    assertTrue(indexer.isStop)
    assertEquals(expected, indexer.latexes.map(_.trim))
  }
}