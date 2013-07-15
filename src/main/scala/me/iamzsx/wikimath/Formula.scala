package me.iamzsx.wikimath

import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.payloads.PayloadTermQuery
import org.apache.lucene.util.BytesRef
import org.apache.lucene.index.Term
import me.iamzsx.xyz.TermLevelPayloadFunction
import org.apache.lucene.search.BooleanClause
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
import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.search.similarities.DefaultSimilarity
import java.io.IOException
import javax.xml.stream.XMLInputFactory
import java.io.StringReader
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants
import scala.collection.mutable.ArrayBuffer

case class FormulaTerm(
  val term: String,
  val level: Int,
  val generalization: Boolean) {

  def normalizeScore = if (generalization) 0.5F else 1F

  def toPayload = new BytesRef(PayloadHelper.encodeInt(level))

  def toTermQuery = {
    val termQuery = new PayloadTermQuery(
      new Term("formula", term),
      new TermLevelPayloadFunction(level))
    termQuery.setBoost(normalizeScore)
    termQuery
  }
}

class FormulaQueryParser {

  def parse(query: String): Query = {
    val combinedQuery = new BooleanQuery(true) // TODO true or false
    val formulaTerm = new FormulaTerm(query, 3, false)
    combinedQuery.add(formulaTerm.toTermQuery, BooleanClause.Occur.SHOULD)
    combinedQuery
  }
}

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

class Tag(val parent: Tag, val label: String) {
  val children = ArrayBuffer[Tag]()

  def add(node: Tag) {
    children += node
  }

  def isText = false

  def isOnlyText = {
    children.isEmpty || children.forall(child => {
      child.label == "mo" || child.label == "mi" || child.isText
    })
  }

  def toLabelString = "<" + label + "></" + label + ">"

  override def toString = "<" + label + ">" + children.map(_.toString).mkString + "</" + label + ">"
}

class Text(parent: Tag, label: String) extends Tag(parent, label) {
  override def isText = true
  override def toString = label
}

// Thread-safe?
class FormulaTokenizer(_input: Reader) extends Tokenizer(_input) {
  val engine = new SnuggleEngine

  val xmlInputFactory = XMLInputFactory.newInstance()
  xmlInputFactory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false);

  val options = new XMLStringOutputOptions
  options.setSerializationMethod(SerializationMethod.XML)
  options.setIndenting(true)
  options.setEncoding("UTF-8")
  options.setAddingMathSourceAnnotations(false)
  options.setUsingNamedEntities(true)

  val termAtt: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  val payloadAtt: PayloadAttribute = addAttribute(classOf[PayloadAttribute])

  var tokens = ListBuffer[FormulaTerm]()

  override def incrementToken = {
    if (tokens.isEmpty) {
      false
    } else {
      val formulaTerm = tokens.head
      termAtt.setEmpty().append(formulaTerm.term)
      payloadAtt.setPayload(formulaTerm.toPayload)
      tokens = tokens.tail
      true
    }
  }

  override def reset {
    val session = engine.createSession
    val latex = IOUtils.toString(input)
    val latexInput = new SnuggleInput("$$ " + latex + " $$")
    if (!session.parseInput(latexInput)) {
      throw new IOException("Parse error: " + latexInput)
    }
    val www = session.buildXMLString(options)
      .replace("""<math xmlns="http://www.w3.org/1998/Math/MathML"""", "<math ")

    println(www)

    val node = xmlToTree(www)
    println(node)

    toTokens(node.children(0), 1)
  }

  def xmlToTree(xml: String): Tag = {
    val streamReader = xmlInputFactory.createXMLStreamReader(new StringReader(xml))
    var node = new Tag(null, "")
    while (streamReader.hasNext()) {
      streamReader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          val childNode = new Tag(node, streamReader.getLocalName())
          node.add(childNode)
          node = childNode
        case XMLStreamConstants.CHARACTERS =>
          node.add(new Text(node, streamReader.getText.trim))
        case XMLStreamConstants.ENTITY_REFERENCE =>
          node.add(new Text(node, "&" + streamReader.getLocalName + ";"))
        case XMLStreamConstants.END_ELEMENT =>
          node = node.parent
        case _ =>
      }
    }
    node
  }

  def tailor(node: Tag) = {
    node.children(0).toString
  }

  def toTokens(node: Tag, level: Int): String = {
    if (node.label == "mo" || node.label == "mi") {
      tailor(node)
    } else if (node.isText) {
      node.toString
    } else {
      if (!node.isOnlyText) {
        val g = node.children.map(child => {
          if (child.label == "mo" || child.label == "mi")
            tailor(child)
          else
            child.toLabelString
        }).mkString
        val formulaTerm = new FormulaTerm(g, level, true)
        tokens += formulaTerm
        val t = node.children.map(child => {
          if (child.label == "mo" || child.label == "mi")
            tailor(child)
          else
            toTokens(child, level + 1)
        }).mkString
        val formulaTerm1 = new FormulaTerm(t, level, false)
        tokens += formulaTerm1
        "<" + node.label + ">" + t + "</" + node.label + ">"
      } else {
        val t = node.children.map(child => {
          if (child.label == "mo" || child.label == "mi")
            tailor(child)
          else
            child.toLabelString
        }).mkString
        "<" + node.label + ">" + t + "</" + node.label + ">"
      }
    }
  }

  override def end {
    super.end
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


