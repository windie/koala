package com.linxiaoyan.wikimirs

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReconstructExpressionVistorSuite extends FunSuite {

  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  val visitor1 = new AdjustTreeVisitor
  val visitor = new ReconstructExpressionVistor

  def convert(latex: String): MO = {
    val tokens = ListBuffer[FormulaTerm]()
    val mathml = latex2mathml.toMathml(latex)
    println(mathml)
    val node = builder.parse(mathml)
    visitor1.visit(node).asInstanceOf[MathmlTag].children(0).asInstanceOf[MO]
  }

  test("latex: +") {
    val mo = convert("+")
    assertEquals(2, visitor.priority(null, mo))
  }

  test("latex: -") {
    val mo = convert("-")
    assertEquals(2, visitor.priority(null, mo))
  }

  test("latex: a +") {
    val mo = convert("+")
    assertEquals(5, visitor.priority(new MI("a"), mo))
  }

  test("latex: a -") {
    val mo = convert("-")
    assertEquals(5, visitor.priority(new MI("a"), mo))
  }

  test("latex: *") {
    val mo = convert("*")
    assertEquals(4, visitor.priority(null, mo))
  }

  test("latex: /") {
    val mo = convert("/")
    assertEquals(4, visitor.priority(null, mo))
  }

  //  test("latex: %") {
  //    val mo = convert("a \\mod b")
  //    assertEquals(4, visitor.priority(null, mo))
  //  }

  test("latex: <") {
    val mo = convert("<")
    assertEquals(6, visitor.priority(null, mo))
  }

  test("latex: >") {
    val mo = convert(">")
    assertEquals(6, visitor.priority(null, mo))
  }

  test("latex: <=") {
    val mo = convert("<=")
    assertEquals(6, visitor.priority(null, mo))
  }

  test("latex: >=") {
    val mo = convert(">=")
    assertEquals(6, visitor.priority(null, mo))
  }

  test("latex: \\leq") {
    val mo = convert("\\leq")
    assertEquals(6, visitor.priority(null, mo))
  }

  test("latex: \\geq") {
    val mo = convert("\\geq")
    assertEquals(6, visitor.priority(null, mo))
  }
  //  test("latex: ==") {
  //    val mo = convert("==")
  //    assertEquals(6, visitor.priority(null, mo))
  //  }
  //
  //  test("latex: !=") {
  //    val mo = convert("!=")
  //    assertEquals(6, visitor.priority(null, mo))
  //  }
}

@RunWith(classOf[JUnitRunner])
class ReconstructExpressionVistorSuite2 extends FunSuite {

  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  val visitor1 = new AdjustTreeVisitor
  val visitor = new ReconstructExpressionVistor

  def convert(latex: String): MathmlNode = {
    val tokens = ListBuffer[FormulaTerm]()
    val mathml = latex2mathml.toMathml(latex)
    val node = builder.parse(mathml)
    println(node)
    visitor.visit(visitor1.visit(node))
  }

  def assertCase(expected: String, latex: String) {
    val node = convert(latex)
    assertEquals(expected, node.toString)
  }

  test("latex: a + b") {
    assertCase("<math><mo o='+'><mi>a</mi><mi>b</mi></mo></math>", "a + b")
  }

  test("latex: a + b + c") {
    assertCase("<math><mo o='+'><mo o='+'><mi>a</mi><mi>b</mi></mo><mi>c</mi></mo></math>", "a + b + c")
  }

  test("latex: a * b + c") {
    assertCase("<math><mo o='+'><mo o='*'><mi>a</mi><mi>b</mi></mo><mi>c</mi></mo></math>", "a * b + c")
  }

  test("latex: a + b * c") {
    assertCase("<math><mo o='+'><mi>a</mi><mo o='*'><mi>b</mi><mi>c</mi></mo></mo></math>", "a + b * c")
  }

  test("latex: a * b + c * d") {
    assertCase("<math><mo o='+'><mo o='*'><mi>a</mi><mi>b</mi></mo><mo o='*'><mi>c</mi><mi>d</mi></mo></mo></math>", "a * b + c * d")
  }

  test("latex: a * ( b + c )") {
    assertCase("<math><mo o='*'><mi>a</mi><mfenced><mrow><mo o='+'><mi>b</mi><mi>c</mi></mo></mrow></mfenced></mo></math>", "a * ( b + c )")
  }

  test("latex: a + b / c") {
    assertCase("<math><mo o='+'><mi>a</mi><mo o='/'><mi>b</mi><mi>c</mi></mo></mo></math>", "a + b / c")
  }

  test("latex: a + b = c") {
    assertCase("<math><mo o='='><mo o='+'><mi>a</mi><mi>b</mi></mo><mi>c</mi></mo></math>", "a + b = c")
  }

  test("latex: \\frac{1}{n} + b = c") {
    assertCase("<math><mo o='='><mo o='+'><mfrac><mn>1</mn><mi>n</mi></mfrac><mi>b</mi></mo><mi>c</mi></mo></math>", "\\frac{1}{n} + b = c")
  }

  test("latex: \\frac{1}{n} + 2*n = a+b") {
    assertCase("<math><mo o='='><mo o='+'><mfrac><mn>1</mn><mi>n</mi></mfrac><mo o='*'><mn>2</mn><mi>n</mi></mo></mo><mo o='+'><mi>a</mi><mi>b</mi></mo></mo></math>", "\\frac{1}{n} + 2*n = a+b")
  }
}