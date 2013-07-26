package com.linxiaoyan.wikimirs

import java.io.StringReader

import scala.collection.mutable.ListBuffer

import org.apache.xerces.parsers.SAXParser
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.xml.sax.InputSource

@RunWith(classOf[JUnitRunner])
class FormulaSuite extends FunSuite {

  val parser2 = new SAXParser
  parser2.setFeature("http://xml.org/sax/features/validation", false);
  parser2.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
  parser2.setFeature("http://xml.org/sax/features/namespaces", false)
  parser2.setFeature("http://apache.org/xml/features/validation/unparsed-entity-checking", false)
  parser2.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)

  test("latex: a") {

    val handler = new MathmlXMLHandler
    parser2.setContentHandler(handler);
    parser2.setErrorHandler(handler);
    val mathml = "<math><mi>a</mi></math>"
    val expected = "a"
    parser2.parse(new InputSource(new StringReader(mathml)))
    val tokens = ListBuffer[FormulaTerm]()
    handler.toTokens(handler.node.children(0), 1, tokens)
  }

  test("latex: ab") {
    val handler = new MathmlXMLHandler
    parser2.setContentHandler(handler);
    parser2.setErrorHandler(handler);
    val mathml = "<math><mi>a</mi><mi>b</mi></math>"
    val expected = "ab"
    parser2.parse(new InputSource(new StringReader(mathml)))
    val tokens = ListBuffer[FormulaTerm]()
    handler.toTokens(handler.node.children(0), 1, tokens)
    println(tokens)
  }

  test("latex: x ^ y") {
    val handler = new MathmlXMLHandler
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup></math>"
    val expected = List("<msup></msup>", "<msup>xy</msup>")
  }

  // <math><mi>a</mi></math>
  // <math><mi>a</mi><mi>b</mi></math>
}