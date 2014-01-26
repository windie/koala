package com.linxiaoyan.wikimirs.lab

import scala.math._
import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import scala.io.Source
import java.io._
import uk.ac.ed.ph.snuggletex.XMLStringOutputOptions
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.SerializationMethod
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import scala.collection.mutable.ListBuffer
import com.linxiaoyan.wikimirs.{ MathmlNode, MathmlTag, MI, MO, MN, NaiveNode, Text, LatexToMathml, AdjustTreeVisitor, MathmlBuilder, ErrorCount }


@RunWith(classOf[JUnitRunner])
class EliminatorSuite extends FunSuite {
  val miEliminator = new MIEliminator
  val mnEliminator = new MNEliminator
  val mimnEliminator = new MIMNEliminator

  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  val visitor = new AdjustTreeVisitor

  def convert(latex: String): MathmlNode = {
    val tokens = ListBuffer[LabTerm]()
    val mathml = latex2mathml.toMathml(latex)
    val node = builder.parse(mathml)
    println(node)
    visitor.visit(node)
  }

  test("latex: a + 1") {
    val latex = "a + 1"
    assertEquals("<math><mi></mi><mo o='+'></mo><mn>1</mn></math>", miEliminator.visit(convert(latex)).toString)
    assertEquals("<math><mi>a</mi><mo o='+'></mo><mn></mn></math>", mnEliminator.visit(convert(latex)).toString)
    assertEquals("<math><mi></mi><mo o='+'></mo><mn></mn></math>", mimnEliminator.visit(convert(latex)).toString)
  }

  test("latex: (a + 1) * (b + 1)") {
    val latex = "(a + 1) * (b + 1)"
    assertEquals("<math><mfenced><mrow><mi></mi><mo o='+'></mo><mn>1</mn></mrow></mfenced><mo o='*'></mo><mfenced><mrow><mi></mi><mo o='+'></mo><mn>1</mn></mrow></mfenced></math>", miEliminator.visit(convert(latex)).toString)
    assertEquals("<math><mfenced><mrow><mi>a</mi><mo o='+'></mo><mn></mn></mrow></mfenced><mo o='*'></mo><mfenced><mrow><mi>b</mi><mo o='+'></mo><mn></mn></mrow></mfenced></math>", mnEliminator.visit(convert(latex)).toString)
    assertEquals("<math><mfenced><mrow><mi></mi><mo o='+'></mo><mn></mn></mrow></mfenced><mo o='*'></mo><mfenced><mrow><mi></mi><mo o='+'></mo><mn></mn></mrow></mfenced></math>", mimnEliminator.visit(convert(latex)).toString)
  }
}

@RunWith(classOf[JUnitRunner])
class SubtreeTokenizerSuite extends FunSuite {
  val latex2mathml = new LatexToMathml
  val builder = new MathmlBuilder
  val visitor = new AdjustTreeVisitor
  val tokenizer = new SubtreeTokenizer(0.5)

  def convert(latex: String): MathmlNode = {
    val tokens = ListBuffer[LabTerm]()
    val mathml = latex2mathml.toMathml(latex)
    val node = builder.parse(mathml)
    println(node)
    visitor.visit(node)
  }

  test("latex: a + 1") {
    val latex = "a + 1"
    val expected = List(
      new LabTerm("<mi>a</mi><mo o='+'></mo><mn>1</mn>", 1, 0.5),
      new LabTerm("<mo o='+'></mo>", 2, 0.5))

    val tokens = ListBuffer[LabTerm]()
    tokenizer.toTokens(convert(latex), tokens)
    assertEquals(expected, tokens)
  }

  test("latex: (a + 1) * (b + 1)") {
    val latex = "(a + 1) * (b + 1)"
    val expected = List(
      new LabTerm("<mfenced><mrow><mi>a</mi><mo o='+'></mo><mn>1</mn></mrow></mfenced><mo o='*'></mo><mfenced><mrow><mi>b</mi><mo o='+'></mo><mn>1</mn></mrow></mfenced>", 1, 0.5),
      new LabTerm("<mfenced><mrow><mi>a</mi><mo o='+'></mo><mn>1</mn></mrow></mfenced>", 2, 0.5),
      new LabTerm("<mi>a</mi><mo o='+'></mo><mn>1</mn>", 3, 0.5),
      new LabTerm("<mo o='+'></mo>", 4, 0.5),
      new LabTerm("<mo o='*'></mo>", 2, 0.5),
      new LabTerm("<mfenced><mrow><mi>b</mi><mo o='+'></mo><mn>1</mn></mrow></mfenced>", 2, 0.5),
      new LabTerm("<mi>b</mi><mo o='+'></mo><mn>1</mn>", 3, 0.5),
      new LabTerm("<mo o='+'></mo>", 4, 0.5))

    val tokens = ListBuffer[LabTerm]()
    tokenizer.toTokens(convert(latex), tokens)
    assertEquals(expected, tokens)
  }
}

