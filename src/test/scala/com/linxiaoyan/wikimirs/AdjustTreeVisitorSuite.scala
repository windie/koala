package com.linxiaoyan.wikimirs

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AdjustTreeVisitorSuite extends FunSuite {

  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  val visitor = new AdjustTreeVisitor

  def convert(latex: String): MathmlNode = {
    val tokens = ListBuffer[FormulaTerm]()
    val mathml = latex2mathml.toMathml(latex)
    val node = builder.parse(mathml)
    println(node)
    visitor.visit(node)
  }

  def assertCase(expected: String, latex: String) {
    val node = convert(latex)
    assertEquals(expected, node.toString)
  }

  test("latex: a") {
    assertCase("<math><mi>a</mi></math>", "a")
  }

  test("latex: ab") {
    assertCase("<math><mi>ab</mi></math>", "ab")
  }

  test("latex: a+b") {
    assertCase("<math><mi>a</mi><mo o='+'></mo><mi>b</mi></math>", "a+b")
  }

  test("latex: 123") {
    assertCase("<math><mn>123</mn></math>", "123")
  }

  test("latex: sin(x)") {
    assertCase("<math><mi>sin</mi><mfenced><mi>x</mi></mfenced></math>", "sin(x)")
  }

  test("latex: x ^ y + z") {
    assertCase("<math><msup><mi>x</mi><mi>y</mi></msup><mo o='+'></mo><mi>z</mi></math>", "x ^ y + z")
  }

  test("latex: x_n") {
    assertCase("<math><msub><mi>x</mi><mi>n</mi></msub></math>", "x_n")
  }

  test("latex: \\sqrt{text}") {
    assertCase("<math><msqrt><mi>text</mi></msqrt></math>", "\\sqrt{text}")
  }

  test("latex: \\frac{a}{b}") {
    assertCase("<math><mfrac><mi>a</mi><mi>b</mi></mfrac></math>", "\\frac{a}{b}")
  }

  test("latex: a + b .") {
    assertCase("<math><mi>a</mi><mo o='+'></mo><mi>b</mi></math>", "a + b .")
  }

  test("latex: a + b \\,") {
    assertCase("<math><mi>a</mi><mo o='+'></mo><mi>b</mi></math>", "a + b \\,")
  }

  test("latex: \\sum_{i=0}^5") {
    assertCase("<math><munderover><mo o='&Sum;'></mo><mi>i</mi><mi>n</mi></munderover></math>", "\\sum_{i}^{n}")
  }

  test("latex: \\root_{i}^{j}") {
    assertCase("<math><msubsup><mrow></mrow><mi>i</mi><mi>j</mi></msubsup></math>", "\\root_{i}^{j}")
  }
}