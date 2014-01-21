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