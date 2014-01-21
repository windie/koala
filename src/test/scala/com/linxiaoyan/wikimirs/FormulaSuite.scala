package com.linxiaoyan.wikimirs

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FormulaSuite extends FunSuite {

  val parser = new MathmlParser(true, true, true)

  test("latex: a") {
    val mathml = "<math><mi>a</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>a</mi>", 1, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: ||") {
    val mathml = "<math><mi>&shortparallel;</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>&shortparallel;</mi>", 1, false),
      new FormulaTerm("&shortparallel;", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: a + b") {
    val mathml = "<math><mi>a</mi><mo>+</mo><mi>b</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>&shortparallel;</mi>", 1, false),
      new FormulaTerm("&shortparallel;", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: ab") {
    val mathml = "<math><mi>a</mi><mi>b</mi></math>"
    val expected = List(new FormulaTerm("<mi></mi>", 1, true),
      new FormulaTerm("<mi>ab</mi>", 1, false),
      new FormulaTerm("ab", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: x ^ y") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup></math>"
    val expected = List(new FormulaTerm("<msup></msup>", 1, true),
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 2, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 2, false),
      new FormulaTerm("<mi>x</mi>", 3, false),
      new FormulaTerm("<mi>y</mi>", 3, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: x ^ y + z") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup><mo>+</mo><mi>z</mi></math>"
    val expected = List(new FormulaTerm("<msup></msup><mo>+</mo><mi></mi>", 1, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup><mo>+</mo><mi>z</mi>", 1, false),
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 2, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

}