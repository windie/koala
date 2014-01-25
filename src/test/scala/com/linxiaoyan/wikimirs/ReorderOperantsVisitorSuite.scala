package com.linxiaoyan.wikimirs

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import org.junit.Assert._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReorderOperantsVisitorSuite extends FunSuite {

  val visitor = new ReorderOperantsVisitor

  test("latex: a + b") {
    val node = new MO("+", List(new MI("a"), new MI("b")))
    assertEquals("<mo o='+'><mi>a</mi><mi>b</mi></mo>", visitor.visit(node).toString)
  }

  test("latex: b + a") {
    val node = new MO("+", List(new MI("b"), new MI("a")))
    assertEquals("<mo o='+'><mi>a</mi><mi>b</mi></mo>", visitor.visit(node).toString)
  }

  test("latex: a * b") {
    val node = new MO("*", List(new MI("a"), new MI("b")))
    assertEquals("<mo o='*'><mi>a</mi><mi>b</mi></mo>", visitor.visit(node).toString)
  }

  test("latex: b * a") {
    val node = new MO("*", List(new MI("b"), new MI("a")))
    assertEquals("<mo o='*'><mi>a</mi><mi>b</mi></mo>", visitor.visit(node).toString)
  }

}

@RunWith(classOf[JUnitRunner])
class ReorderOperantsVisitorSuite2 extends FunSuite {
  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  private[this] val visitors = {
    val visitors = ListBuffer[TreeVisitor]()
    visitors += new AdjustTreeVisitor
    visitors += new ReconstructExpressionVistor
    visitors += new ReorderOperantsVisitor
    visitors.toList
  }

  def convert(latex: String): String = {
    val mathml = latex2mathml.toMathml(latex)
    val node =
      visitors.foldLeft(builder.parse(mathml)) {
        case (node, visitor) => {
          visitor.visit(node)
        }
      }
    node.toString
  }

  test("latex: x + y") {
    val e = "<math><mo o='+'><mi>x</mi><mi>y</mi></mo></math>"
    assertEquals(e, convert("x + y"))
    assertEquals(e, convert("y + x"))
  }

  test("latex: e ^ {x + y}") {
    val e = "<math><msup><mi>e</mi><mrow><mo o='+'><mi>x</mi><mi>y</mi></mo></mrow></msup></math>";
    assertEquals(e, convert(" e ^ {x + y}"))
    assertEquals(e, convert(" e ^ {y + x}"))
  }

  test("latex:  (x + y) ^ z") {
    val e = "<math><msup><mfenced><mrow><mo o='+'><mi>x</mi><mi>y</mi></mo></mrow></mfenced><mi>z</mi></msup></math>";
    assertEquals(e, convert("(x + y) ^ z"))
    assertEquals(e, convert("(y + x) ^ z"))
  }

  test("latex: (z + y) + x") {
    val e = "<math><mo o='+'><mfenced><mrow><mo o='+'><mi>y</mi><mi>z</mi></mo></mrow></mfenced><mi>x</mi></mo></math>";
    assertEquals(e, convert("(z + y) + x"))
    assertEquals(e, convert("(y + z) + x"))
    assertEquals(e, convert("x + (z + y)"))
    assertEquals(e, convert("x + (y + z)"))
  }

  test("latex: \\cos{x + y}") {
    val e = "<math><mo o='cos'><mrow><mo o='+'><mi>x</mi><mi>y</mi></mo></mrow></mo></math>"
    assertEquals(e, convert("\\cos{x + y}"))
    assertEquals(e, convert("\\cos{y + x}"))
  }

  test("latex: \\cos{x * y}") {
    val e = "<math><mo o='cos'><mrow><mo o='*'><mi>x</mi><mi>y</mi></mo></mrow></mo></math>"
    assertEquals(e, convert("\\cos{x * y}"))
    assertEquals(e, convert("\\cos{y * x}"))
  }

  test("latex: (z * y) + x") {
    val e = "<math><mo o='+'><mfenced><mrow><mo o='*'><mi>y</mi><mi>z</mi></mo></mrow></mfenced><mi>x</mi></mo></math>";
    assertEquals(e, convert("(z * y) + x"))
    assertEquals(e, convert("(y * z) + x"))
    assertEquals(e, convert("x + (z * y)"))
    assertEquals(e, convert("x + (y * z)"))
  }
}