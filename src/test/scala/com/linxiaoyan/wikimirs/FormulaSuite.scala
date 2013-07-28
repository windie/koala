package com.linxiaoyan.wikimirs

import java.io.StringReader

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

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
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>a</mi>", 1, false))

    val tokens = new java.util.LinkedList[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens.asScala)
  }

  test("latex: ||") {
    val mathml = "<math><mi>&shortparallel;</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>&shortparallel;</mi>", 1, false),
      new FormulaTerm("&shortparallel;", 2, false))

    val tokens = new java.util.LinkedList[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens.asScala)
  }

  test("latex: ab") {
    val mathml = "<math><mi>a</mi><mi>b</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi><mi></mi>", 1, true),
      new FormulaTerm("<mi>a</mi><mi>b</mi>", 1, false))

    val tokens = new java.util.LinkedList[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens.asScala)
  }

  test("latex: x ^ y") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup></math>"
    val expected = List(new FormulaTerm("<msup></msup>", 1, true),
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 2, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 2, false))

    val tokens = new java.util.LinkedList[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens.asScala)
  }

  test("latex: x ^ y + z") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup><mo>+</mo><mi>z</mi></math>"
    val expected = List(new FormulaTerm("<msup></msup><mo>+</mo><mi></mi>", 1, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup><mo>+</mo><mi>z</mi>", 1, false),
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 2, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 2, false))

    val tokens = new java.util.LinkedList[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens.asScala)
  }

}