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
    val expected = List()

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: a + b") {
    val mathml = "<math><mi>a</mi><mo>+</mo><mi>b</mi></math>"
    val expected = List(
      new FormulaTerm("<mo o='+'><mi></mi><mi></mi></mo>", 2, true),
      new FormulaTerm("<mo o='+'><mi>a</mi><mi>b</mi></mo>", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: ||") {
    val mathml = "<math><mi>&shortparallel;</mi></math>"
    val expected = List(
      new FormulaTerm("<mi>&shortparallel;</mi>", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: ab") {
    val mathml = "<math><mi>a</mi><mi>b</mi></math>"
    val expected = List(
      new FormulaTerm("<mi>ab</mi>", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: x ^ y") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup></math>"
    val expected = List(
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 2, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 2, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

  test("latex: x ^ y + z") {
    val mathml = "<math><msup><mi>x</mi><mi>y</mi></msup><mo>+</mo><mi>z</mi></math>"
    val expected = List(new FormulaTerm("<mo o='+'><mi></mi><msup></msup></mo>", 2, true),
      new FormulaTerm("<mo o='+'><mi>z</mi><msup><mi>x</mi><mi>y</mi></msup></mo>", 2, false),
      new FormulaTerm("<msup><mi></mi><mi></mi></msup>", 3, true),
      new FormulaTerm("<msup><mi>x</mi><mi>y</mi></msup>", 3, false))

    val tokens = ListBuffer[FormulaTerm]()
    parser.parse(mathml, tokens)
    assertEquals(expected, tokens)
  }

}

@RunWith(classOf[JUnitRunner])
class LatexToMathmlSuite extends FunSuite {
  val latexToMathml = new LatexToMathml
  val builder = new MathmlBuilder

  def convert(latex: String): String = {
    builder.parse(latexToMathml.toMathml(latex)).toString
  }

  test("latex: p \\to q") {
    assertEquals("<math><mi>p</mi><mo>&ShortRightArrow;</mo><mi>q</mi></math>", convert("p \\to q"))
  }

  test("latex: p \\rightarrow q") {
    assertEquals("<math><mi>p</mi><mo>&ShortRightArrow;</mo><mi>q</mi></math>", convert("p \\rightarrow q"))
  }

  test("latex: p \\choose q") {
    assertEquals("<math><mi>p</mi><mo>choose</mo><mi>q</mi></math>", convert("p \\choose q"))
  }
}