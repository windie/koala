package me.iamzsx.wikimath

import java.io.Reader
import scala.collection.mutable.ListBuffer
import scala.xml._
import scala.xml.Node
import scala.xml.NodeSeq
import org.apache.commons.io.IOUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.tokenattributes.FlagsAttribute
import org.apache.lucene.analysis.tokenattributes.PayloadAttribute
import org.apache.lucene.analysis.tokenattributes.PositionLengthAttribute
import uk.ac.ed.ph.snuggletex.SerializationMethod
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.XMLStringOutputOptions
import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.analysis.payloads.DelimitedPayloadTokenFilter
import org.apache.lucene.util.BytesRef
import org.apache.lucene.analysis.payloads.PayloadHelper;  
import org.apache.lucene.search.similarities.DefaultSimilarity;  

class FormulaAnalyzer extends Analyzer {

  override def createComponents(fieldName: String,
    reader: Reader): TokenStreamComponents = {
    val tokenizer = new FormulaTokenizer(reader)
    new TokenStreamComponents(tokenizer, tokenizer) {
      override def setReader(reader: Reader) {
        super.setReader(reader);
      }
    };
  }
}

// Thread-safe?
class FormulaTokenizer(input: Reader) extends Tokenizer(input) {
  val engine = new SnuggleEngine

  val options = new XMLStringOutputOptions
  options.setSerializationMethod(SerializationMethod.XML)
  options.setIndenting(true)
  options.setEncoding("UTF-8")
  options.setAddingMathSourceAnnotations(false)
  options.setUsingNamedEntities(true)
  options.setDoctypeSystem("""http://www.w3.org/Math/DTD/mathml3/mathml3.dtd""")

  val termAtt: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  val levelAtt: PositionLengthAttribute = addAttribute(classOf[PositionLengthAttribute])
  val generalizationAtt: FlagsAttribute = addAttribute(classOf[FlagsAttribute])
  val payloadAtt: PayloadAttribute = addAttribute(classOf[PayloadAttribute])

  case class Token(
    val term: String,
    val level: Int,
    val generalization: Boolean)

  var tokens: List[Token] = Nil

  override def incrementToken = {
    if (tokens.isEmpty) {
      false
    } else {
      val token = tokens.head
      termAtt.copyBuffer(token.term.toCharArray(), 0, token.term.length)
      val payload = PayloadHelper.encodeInt(token.level)
      payloadAtt.setPayload(new BytesRef(payload))
      tokens = tokens.tail
      true
    }
  }

  override def reset {
    //    val session = engine.createSession
    //    println(System.currentTimeMillis / 1000)
    //    val latex = IOUtils.toString(input)
    //    val latexInput = new SnuggleInput("$$ " + latex + " $$")
    //    if (!session.parseInput(latexInput)) {
    //      throw new IOException("Parse error: " + latexInput)
    //    }
    //    val www = session.buildXMLString(options)
    //    println(www)
    //    println(System.currentTimeMillis / 1000)
    //    val xml = XML.loadString(www)
    //    println(System.currentTimeMillis / 1000)
    //    xml match {
    //      case <math>{ contents @ _* }</math> => {
    //        tokens = toTokens(ListBuffer[Token](), contents, 1).toList
    //      }
    //      case _ => throw new IOException("Invalid xml: " + xml)
    //    }
    //    println(System.currentTimeMillis / 1000)
    val latex = IOUtils.toString(input)
    tokens = latex.split(" ").map(new Token(_, 1, false)).toList
  }

  private def toTokens(buffer: ListBuffer[Token], nodes: NodeSeq, level: Int): ListBuffer[Token] = {
    nodes.foreach { node =>
      buffer += new Token(Utility.serialize(node, pscope = node.scope).toString, level, false)
    }
    buffer
  }

  override def end {
  }

  override def close {
    super.close
  }
}

object Test {
  def a = {

    val engine = new SnuggleEngine
    val session = engine.createSession

    val input = new SnuggleInput("""$$ s_n(T) = \inf\big\{\, \|T-L\| : L\text{ is an operator of finite rank }<n \,\big\}. $$""");
    session.parseInput(input);

    val options = new XMLStringOutputOptions
    options.setSerializationMethod(SerializationMethod.XML)
    options.setIndenting(true)
    options.setEncoding("UTF-8")
    options.setAddingMathSourceAnnotations(false)
    options.setUsingNamedEntities(true)
    options.setDoctypeSystem("""http://www.w3.org/Math/DTD/mathml3/mathml3.dtd""")

    // XML.loadString( + session.buildXMLString(options))
    session.buildXMLString(options)
  }

  def main(args: Array[String]) {
    val x = <a>&nbps;</a>

    println(x.child)
  }

  def b = {
    val x = <a><b>1</b><c></c></a>
    x match {
      case <a>{ y @ _* }</a> => println(y)
      case _ => println()
    }
  }
}
