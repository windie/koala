package com.linxiaoyan.wikimirs

import java.io.StringReader

import scala.collection.mutable.ListBuffer

import org.apache.xerces.parsers.SAXParser
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.xml.sax.InputSource
import org.junit.Assert._

@RunWith(classOf[JUnitRunner])
class FormulaSuite extends FunSuite {

  val parser = new MathmlParser

  test("latex: a") {
    val mathml = "<math><mi>a</mi></math>"
    val expected = "<mi>a</mi>"

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertTrue(tokens.isEmpty)
  }

  test("latex: ||") {
    val mathml = "<math><mi>&shortparallel;</mi></math>"
    val expected = "&shortparallel;"

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(1, tokens.size)
    assertEquals(new FormulaTerm(expected, 1, false), tokens.head)
  }

  test("latex: ab") {
    val mathml = "<math><mi>a</mi><mi>b</mi></math>"
    val expected = "ab"

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(1, tokens.size)
    assertEquals(new FormulaTerm(expected, 1, false), tokens.head)
  }

  test("latex: x ^ y") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup></math>"
    val expected = List("<msup></msup>", "<msup>xy</msup>", "xy")

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    println(tokens)
  }

}