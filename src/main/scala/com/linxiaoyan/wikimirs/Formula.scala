package com.linxiaoyan.wikimirs

import java.io.File
import java.io.IOException
import java.io.Reader
import java.io.StringReader
import java.net.URLEncoder
import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.tokenattributes.FlagsAttribute
import org.apache.lucene.analysis.tokenattributes.PayloadAttribute
import org.apache.lucene.document.Document
import org.apache.lucene.index.FieldInvertState
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.LogByteSizeMergePolicy
import org.apache.lucene.index.Norm
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Explanation
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.payloads.PayloadFunction
import org.apache.lucene.search.payloads.PayloadTermQuery
import org.apache.lucene.search.similarities.DefaultSimilarity
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.BytesRef
import org.apache.lucene.util.BytesRef
import org.apache.lucene.util.ToStringUtils
import org.apache.lucene.util.Version
import org.apache.xerces.parsers.SAXParser
import org.apache.lucene.search.TopDocs
import org.xml.sax.Attributes
import org.xml.sax.InputSource
import org.xml.sax.SAXParseException
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import uk.ac.ed.ph.snuggletex.SerializationMethod
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.XMLStringOutputOptions
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.search.IndexSearcher
import com.typesafe.scalalogging.slf4j.Logging
import org.apache.lucene.store._

class TermLevelTermQuery(term: Term, function: PayloadFunction) extends PayloadTermQuery(term, function) {

  override def toString(field: String): String = {
    val buffer = new StringBuilder;
    if (!term.field.equals(field)) {
      buffer.append(term.field + ":")
    }
    buffer.append("<code>" + StringEscapeUtils.escapeHtml4(term.text) + "</code>")
    buffer.append(ToStringUtils.boost(getBoost))
    return buffer.toString;
  }
}

case class FormulaTerm(val term: String, val level: Int, val generalization: Boolean) {

  private def getNormalizeScore: Float = if (generalization) 0.5F else 1F

  def toPayload(): BytesRef = new BytesRef(PayloadHelper.encodeInt(level))

  def toTermQuery(): TermLevelTermQuery = {
    val termQuery = new TermLevelTermQuery(new Term("formula",
      term), new FormulaTermLevelPayloadFunction(level));
    termQuery.setBoost(getNormalizeScore);
    termQuery;
  }

  def explain: String = {
    val escapedTerm = StringEscapeUtils.escapeHtml4(term);
    s"""term: <code class='xml'>$escapedTerm</code>, level: <code>$level</code>, generalization: <code>$generalization</code>""";
  }
}

class FormulaQueryParser {

  private val analyzer = new FormulaAnalyzer

  def parse(query: String): Option[Query] = {
    val source = analyzer.tokenStream("formula", new StringReader(query));
    source.reset()
    val combinedQuery = new BooleanQuery(false) // TODO true or false
    var numTokens = 0

    val termAtt: CharTermAttribute = source.getAttribute(classOf[CharTermAttribute])
    val payloadAtt: PayloadAttribute = source.getAttribute(classOf[PayloadAttribute])
    val generalizationAtt: FlagsAttribute = source.getAttribute(classOf[FlagsAttribute])
    while (source.incrementToken()) {
      numTokens += 1
      val formulaTerm = new FormulaTerm(
        termAtt.toString,
        PayloadHelper.decodeInt(payloadAtt.getPayload().bytes, payloadAtt.getPayload().offset),
        generalizationAtt.getFlags() == 1)
      combinedQuery.add(formulaTerm.toTermQuery, BooleanClause.Occur.SHOULD)
    }

    if (numTokens == 0)
      return None

    Some(combinedQuery)
  }

  def explain(query: String): Option[String] = {
    val source = analyzer.tokenStream("formula", new StringReader(query));
    source.reset()
    val combinedQuery = new BooleanQuery(false) // TODO true or false
    var numTokens = 0

    val termAtt: CharTermAttribute = source.getAttribute(classOf[CharTermAttribute])
    val payloadAtt: PayloadAttribute = source.getAttribute(classOf[PayloadAttribute])
    val generalizationAtt: FlagsAttribute = source.getAttribute(classOf[FlagsAttribute])

    val explain = new StringBuilder
    explain ++= "<ul>"
    while (source.incrementToken()) {
      numTokens += 1
      val formulaTerm = new FormulaTerm(
        termAtt.toString,
        PayloadHelper.decodeInt(payloadAtt.getPayload().bytes, payloadAtt.getPayload().offset),
        generalizationAtt.getFlags() == 1)
      explain ++= "<li>"
      explain ++= formulaTerm.explain
      explain ++= "</li>"
    }

    explain ++= "</ul>"
    if (numTokens == 0)
      return None

    Some(explain.toString)
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

trait TreeVisitor {
  def visit(node: MathmlNode): MathmlNode
}

/**
 * <mo>+</mo> -> MO(+)
 * <mi>a</mi> -> MI(a)
 * <mi>a</mi><mi>b</mi> -> MI(ab)
 * <msup><mi>x</mi><mi>y</mi></msup> -> MathmlNode(msup, [MI(x), MI(y)])
 */
class AdjustTreeVisitor extends TreeVisitor {

  private def checkText(node: MathmlTag, text: String): Boolean = {
    (!node.children.isEmpty) && (node.children.head.asInstanceOf[Text].text == ".")
  }

  private def removeLastIfPossible(node: MathmlTag): MathmlTag = {
    if (node.children.isEmpty) {
      return node
    }

    node.children.last match {
      case last: MathmlTag =>
        {
          val removeLast = if (last.tag == "mspace") {
            true
          } else if (last.tag == "mo") {
            checkText(last, ".")
          } else if (last.tag == "mi") {
            checkText(last, " ")
          } else {
            false
          }
          if (removeLast) {
            val newChildren = ListBuffer[MathmlNode]()
            newChildren ++= node.children
            newChildren.remove(newChildren.size - 1)
            new MathmlTag(node.parent, node.tag, newChildren)
          } else {
            node
          }
        }
      case other => node
    }
  }

  def visit(root: MathmlNode): MathmlNode = {
    val updatedNode = root match {
      case node: MathmlTag => {
        removeLastIfPossible(node)
      }
      case other => other
    }
    visitInternal(updatedNode)
  }

  val noMergedTag = "msup" :: "msub" :: "mfrac" :: "munderover" :: "mover" :: "munder" :: "msubsup" :: "mroot" :: Nil

  def visitInternal(root: MathmlNode): MathmlNode = {
    root match {
      case node: MathmlTag => {
        node.tag match {
          case "mo" => {
            assert(node.children.size == 1)
            assert(node.children(0).isInstanceOf[Text])
            new MO(node.children(0).toString)
          }
          case "mi" => {
            assert(node.children.size == 1)
            assert(node.children(0).isInstanceOf[Text])
            new MI(node.children(0).toString)
          }
          case "mn" => {
            assert(node.children.size == 1)
            assert(node.children(0).isInstanceOf[Text])
            new MN(node.children(0).toString)
          }
          case "ms" => {
            assert(node.children.size == 1)
            assert(node.children(0).isInstanceOf[Text])
            new MS(node.children(0).toString)
          }
          case _ => {
            val children = node.children.map(child => visitInternal(child)).foldLeft(ListBuffer[MathmlNode]()) {
              case (children, child) => {
                if (children.isEmpty) {
                  children += child
                } else {
                  if (!noMergedTag.exists { _ == node.tag }) {
                    if (children.last.isInstanceOf[MI] && child.isInstanceOf[MI]) {
                      children.update(children.size - 1, new MI(children.last.asInstanceOf[MI].text + child.asInstanceOf[MI].text));
                    } else {
                      children += child
                    }
                  } else {
                    children += child
                  }
                }
                children
              }
            }
            if (children.size == 1 && node.tag == "mrow") {
              children(0)
            } else {
              new MathmlTag(null, node.tag, children)
            }
          }
        }
      }
      case text: Text =>
        text
    }
  }
}

class ReconstructExpressionVistor extends TreeVisitor {
  def visit(node: MathmlNode): MathmlNode = node match {
    case text: Text => text
    case node: NaiveNode => node
    case parent: MathmlNode =>
      {
        val children = parent.children.map(child => visit(child))
        val operators = new scala.collection.mutable.Stack[MO]
        val stack = new scala.collection.mutable.Stack[MathmlNode]
        var prev: MathmlNode = null
        for (i <- 0 until children.size) {
          val child = children(i)
          if (child.isInstanceOf[MO]) {
            val mo = child.asInstanceOf[MO]
            var operantSize = 0
            if (i != 0 && !children(i - 1).isInstanceOf[MO]) {
              operantSize += 1
            }
            if (i != children.size - 1 && !children(i + 1).isInstanceOf[MO]) {
              operantSize += 1
            }
            mo.operantSize = operantSize
            while (!operators.isEmpty && priority(prev, operators.top) <= priority(prev, mo)) {
              stack push (operators.pop)
            }
            operators push mo
          } else {
            stack push child
          }
          prev = child
        }
        while (!operators.isEmpty) {
          stack push (operators.pop)
        }
        val newChildren = ListBuffer[MathmlNode]()
        while (!stack.isEmpty) {
          newChildren.+=:(buildTree(stack))
        }
        if (parent.isInstanceOf[MO]) {
          new MO(parent.asInstanceOf[MO].text, newChildren.toList)
        } else {
          new MathmlTag(null, parent.asInstanceOf[MathmlTag].tag, newChildren)
        }
      }
    case _ => node
  }

  def buildTree(stack: scala.collection.mutable.Stack[MathmlNode]): MathmlNode = {
    val child = stack.pop
    if (child.isInstanceOf[MO]) {
      var mo = child.asInstanceOf[MO]
      if (mo.operantSize == 1) {
        if (!stack.isEmpty) {
          mo = new MO(mo.text, List(buildTree(stack)))
        }
      } else if (mo.operantSize == 2) {
        var children = List[MathmlNode]()
        if (!stack.isEmpty) {
          children = buildTree(stack) :: children
        }
        if (!stack.isEmpty) {
          children = buildTree(stack) :: children
        }
        mo = new MO(mo.text, children)
      }
      mo
    } else {
      child
    }
  }

  def priority(prev: MathmlNode, mo: MO): Int = {
    mo.text match {
      case "+" =>
        if (prev == null || prev.isInstanceOf[MO]) {
          2
        } else {
          5
        }
      case "-" =>
        if (prev == null || prev.isInstanceOf[MO]) {
          2
        } else {
          5
        }
      case "&not;" => 2
      case "*" => 4
      case "/" => 4
      case "%" => 4 // TODO
      case "mod" => 4
      case "<" => 6
      case ">" => 6
      case "<=" => 6 // TODO
      case "&leq;" => 6
      case "&GreaterEqual;" => 6
      case ">=" => 6 // TODO
      case "==" => 7 // TODO
      case "!=" => 7 // TODO
      case "&vee;" => 8
      case "&wedge;" => 9
      case _ => 100
    }
  }
}

class ReorderOperantsVisitor extends TreeVisitor {
  def visit(node: MathmlNode): MathmlNode = node match {
    case operator: MO =>
      {
        val children = node.children
        if (children.isEmpty) {
          node
        } else {
          val newChildren = ListBuffer[MathmlNode]()
          for (child <- children) {
            newChildren += visit(child)
          }
          if ((operator.text == "+" || operator.text == "*") &&
            newChildren.size == 2 &&
            newChildren(0) > newChildren(1)) {
            new MO(operator.text, List(newChildren(1), newChildren(0)))
          } else {
            new MO(operator.text, newChildren.toList)
          }
        }
      }
    case node: MathmlTag => {
      val children = node.children
      if (children.isEmpty) {
        node
      } else {
        val newChildren = ListBuffer[MathmlNode]()
        for (child <- children) {
          newChildren += visit(child)
        }
        new MathmlTag(node.parent, node.tag, newChildren)
      }
    }
    case node => node
  }
}

class MathmlBuilder {
  private val handler = new MathmlXMLHandler

  private val parser = new SAXParser
  parser.setFeature("http://xml.org/sax/features/validation", false);
  parser.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
  parser.setFeature("http://xml.org/sax/features/namespaces", false)
  parser.setFeature("http://apache.org/xml/features/validation/unparsed-entity-checking", false)
  parser.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)
  parser.setContentHandler(handler)
  parser.setErrorHandler(handler)

  def parse(mathml: String): MathmlNode = {
    handler.clear
    parser.parse(new InputSource(new StringReader(mathml)))
    handler.getTree
  }
}

class MathmlParser(
  enableAdjustTree: Boolean = false,
  enableReconstructExpression: Boolean = false,
  enableReorderOperants: Boolean = false) {

  private val tokenizer = new MathmlTokenizer

  private[this] val visitors = {
    val visitors = ListBuffer[TreeVisitor]()
    if (enableAdjustTree) {
      visitors += new AdjustTreeVisitor
    }
    if (enableReconstructExpression) {
      visitors += new ReconstructExpressionVistor
    }
    if (enableReorderOperants) {
      visitors += new ReorderOperantsVisitor
    }
    visitors.toList
  }

  private val builder = new MathmlBuilder

  def parse(mathml: String, tokens: ListBuffer[FormulaTerm]) {
    val node =
      visitors.foldLeft(builder.parse(mathml)) {
        case (node, visitor) => visitor.visit(node)
      }
    tokenizer.toTokens(node, tokens)
  }
}

import uk.ac.ed.ph.snuggletex.SnugglePackage;
import uk.ac.ed.ph.snuggletex.SnuggleRuntimeException;
import uk.ac.ed.ph.snuggletex.SnugglePackage;
import uk.ac.ed.ph.snuggletex.SnuggleRuntimeException;
import uk.ac.ed.ph.snuggletex.dombuilding.AccentHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.AnchorHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.ArrayHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.BoxHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.CharacterCommandHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.DoNothingHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.EnsureMathHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.EqnArrayHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.GetVarHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.HSpaceHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.HrefHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.InsertUnicodeHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.InterpretableSimpleMathHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.LineBreakHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.ListEnvironmentHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.LiteralHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathComplexCommandHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathEnvironmentHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathFenceHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathLimitsHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathNotHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathRootHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathStackrelHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MathVariantMapHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MatrixHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.ModeDelegatingHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.MrowHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.ParagraphHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.SetVarHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.SimpleXHTMLContainerBuildingHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.SpaceHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.StyleInterpretationHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.TabularHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.TextSafeInterpretableMathIdentifierHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.UnitsHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.VerbatimHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.XMLAttrHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.XMLBlockElementHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.XMLInlineElementHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.XMLNameOrIdHandler;
import uk.ac.ed.ph.snuggletex.dombuilding.XMLUnparseHandler;
import uk.ac.ed.ph.snuggletex.semantics.Interpretation;
import uk.ac.ed.ph.snuggletex.semantics.InterpretationType;
import uk.ac.ed.ph.snuggletex.semantics.MathBigLimitOwnerInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathBracketInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathFunctionInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathIdentifierInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathMLSymbol;
import uk.ac.ed.ph.snuggletex.semantics.MathNegatableInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathOperatorInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.StyleDeclarationInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.TabularInterpretation;
import uk.ac.ed.ph.snuggletex.semantics.MathBracketInterpretation.BracketType;
import uk.ac.ed.ph.snuggletex.tokens.FlowToken;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import uk.ac.ed.ph.snuggletex.definitions._

object WikimirsPackage {
  val extensionPackage: SnugglePackage = {
    synchronized {
      val corePackage = CorePackageDefinitions.getPackage()

      /* Math "functions" (treated as identifiers in MathML) */
      corePackage.addSimpleMathCommand("arccos", new MathOperatorInterpretation("arccos"));
      corePackage.addSimpleMathCommand("arcsin", new MathOperatorInterpretation("arcsin"));
      corePackage.addSimpleMathCommand("arctan", new MathOperatorInterpretation("arctan"));
      corePackage.addSimpleMathCommand("arg", new MathOperatorInterpretation("arg"));
      corePackage.addSimpleMathCommand("cos", new MathOperatorInterpretation("cos"));
      corePackage.addSimpleMathCommand("cosh", new MathOperatorInterpretation("cosh"));
      corePackage.addSimpleMathCommand("cot", new MathOperatorInterpretation("cot"));
      corePackage.addSimpleMathCommand("coth", new MathOperatorInterpretation("coth"));
      corePackage.addSimpleMathCommand("csc", new MathOperatorInterpretation("csc"));
      corePackage.addSimpleMathCommand("deg", new MathOperatorInterpretation("deg"));
      corePackage.addSimpleMathCommand("det", new MathOperatorInterpretation("det"));
      corePackage.addSimpleMathCommand("dim", new MathOperatorInterpretation("dim"));
      corePackage.addSimpleMathCommand("exp", new MathOperatorInterpretation("exp"));
      corePackage.addSimpleMathCommand("gcd", new MathOperatorInterpretation("gcd"));
      corePackage.addSimpleMathCommand("hom", new MathOperatorInterpretation("hom"));
      corePackage.addSimpleMathCommand("inf", new MathOperatorInterpretation("inf"));
      corePackage.addSimpleMathCommand("ker", new MathOperatorInterpretation("ker"));
      corePackage.addSimpleMathCommand("lg", new MathOperatorInterpretation("lg"));
      corePackage.addSimpleMathCommand("lcm", new MathOperatorInterpretation("lcm"));
      corePackage.addSimpleMathCommand("lim", new MathOperatorInterpretation("lim"));
      corePackage.addSimpleMathCommand("liminf", new MathOperatorInterpretation("lim inf"));
      corePackage.addSimpleMathCommand("limsup", new MathOperatorInterpretation("lim sup"));
      corePackage.addSimpleMathCommand("ln", new MathOperatorInterpretation("ln"));
      corePackage.addSimpleMathCommand("log", new MathOperatorInterpretation("log"));
      corePackage.addSimpleMathCommand("max", new MathOperatorInterpretation("max"));
      corePackage.addSimpleMathCommand("min", new MathOperatorInterpretation("min"));
      corePackage.addSimpleMathCommand("Pr", new MathOperatorInterpretation("Pr"));
      corePackage.addSimpleMathCommand("sec", new MathOperatorInterpretation("sec"));
      corePackage.addSimpleMathCommand("sin", new MathOperatorInterpretation("sin"));
      corePackage.addSimpleMathCommand("sinh", new MathOperatorInterpretation("sinh"));
      corePackage.addSimpleMathCommand("sup", new MathOperatorInterpretation("sup"));
      corePackage.addSimpleMathCommand("tan", new MathOperatorInterpretation("tan"));
      corePackage.addSimpleMathCommand("tanh", new MathOperatorInterpretation("tanh"));

      /* Extra Math functions (added for consistency with standard Content MathML operators) */
      corePackage.addSimpleMathCommand("sech", new MathOperatorInterpretation("sech"));
      corePackage.addSimpleMathCommand("csch", new MathOperatorInterpretation("csch"));
      corePackage.addSimpleMathCommand("coth", new MathOperatorInterpretation("coth"));
      corePackage.addSimpleMathCommand("arcsec", new MathOperatorInterpretation("arcsec"));
      corePackage.addSimpleMathCommand("arccsc", new MathOperatorInterpretation("arccsc"));
      corePackage.addSimpleMathCommand("arccot", new MathOperatorInterpretation("arccot"));
      corePackage.addSimpleMathCommand("arccosh", new MathOperatorInterpretation("arccosh"));
      corePackage.addSimpleMathCommand("arcsinh", new MathOperatorInterpretation("arcsinh"));
      corePackage.addSimpleMathCommand("arctanh", new MathOperatorInterpretation("arctanh"));
      corePackage.addSimpleMathCommand("arcsech", new MathOperatorInterpretation("arcsech"));
      corePackage.addSimpleMathCommand("arccsch", new MathOperatorInterpretation("arccsch"));
      corePackage.addSimpleMathCommand("arccoth", new MathOperatorInterpretation("arccoth"));

      val p = new SnugglePackage("wikimirs");
      p.addSimpleMathCommand("mod", new MathOperatorInterpretation("mod"))
      p.addSimpleMathCommand("pmod", new MathOperatorInterpretation("mod"))
      p.addSimpleMathCommand("bmod", new MathOperatorInterpretation("mod"))
      p.addSimpleMathCommand("to", new MathOperatorInterpretation("&ShortRightArrow;"))
      p.addSimpleMathCommand("choose", new MathOperatorInterpretation("choose"))
      p.addSimpleMathCommand("dots", new MathIdentifierInterpretation(MathMLSymbol.CDOTS))
      p
    }
  }
}

class LatexToMathml {
  private val engine = new SnuggleEngine
  engine.addPackage(WikimirsPackage.extensionPackage)

  private val options = new XMLStringOutputOptions
  options.setSerializationMethod(SerializationMethod.XML)
  options.setIndenting(true)
  options.setEncoding("UTF-8")
  options.setAddingMathSourceAnnotations(false)
  options.setUsingNamedEntities(true)

  def toMathml(latex: String) = {
    val session = engine.createSession
    val latexInput = new SnuggleInput("$$ " + latex + " $$")
    if (!session.parseInput(latexInput)) {
      throw new IOException("Parse error: " + latexInput)
    }
    session.buildXMLString(options).replace("""<math xmlns="http://www.w3.org/1998/Math/MathML" display="block"""", "<math")
  }
}

class FormulaTokenizer(_input: Reader) extends Tokenizer(_input) {

  private val latex2mathml = new LatexToMathml

  private val termAtt: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  private val payloadAtt: PayloadAttribute = addAttribute(classOf[PayloadAttribute])
  private val generalizationAtt: FlagsAttribute = addAttribute(classOf[FlagsAttribute])

  private var tokens = ListBuffer[FormulaTerm]()

  private val parser = new MathmlParser(
    enableAdjustTree = Settings.getBoolean("index.enableAdjustTree"),
    enableReconstructExpression = Settings.getBoolean("index.enableReconstructExpression"),
    enableReorderOperants = Settings.getBoolean("index.enableReorderOperants"))

  override def incrementToken = {
    if (tokens.isEmpty) {
      false
    } else {
      val formulaTerm = tokens.head
      termAtt.setEmpty().append(formulaTerm.term)
      payloadAtt.setPayload(formulaTerm.toPayload)
      generalizationAtt.setFlags(if (formulaTerm.generalization) 1 else 0)
      tokens = tokens.tail
      true
    }
  }

  override def reset {
    val latexes = IOUtils.toString(input)
    tokens.clear
    latexes.split('\0').filterNot(_.isEmpty).foreach { latex =>
      try {
        val mathml = latex2mathml.toMathml(latex)
        parser.parse(mathml, tokens)
      } catch {
        case e: Throwable => {
          ErrorCount.count.incrementAndGet()
          if (!e.isInstanceOf[NullPointerException]) {
            println(latex)
            e.printStackTrace
          }
        }
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

class MathmlXMLHandler extends org.xml.sax.helpers.DefaultHandler {

  private var node: MathmlTag = new MathmlTag(null, "dummy")
  var text = new java.lang.StringBuilder

  def getTree = node.children.head

  def clear {
    node = new MathmlTag(null, "dummy")
  }

  override def startElement(uri: String, name: String, qName: String,
    attrs: Attributes) {
    val childNode = new MathmlTag(node, qName)
    node.addChild(childNode)
    node = childNode
  }

  override def endElement(uri: String, name: String, qName: String) {
    if (text.length() != 0) {
      val trim = text.toString.trim;
      if (trim.length() != 0) {
        node.addChild(new Text(node, trim))
      }
      text.setLength(0);
    }
    node = node.parent
  }

  override def characters(ch: Array[Char], start: Int, length: Int) {
    text.append(ch, start, length)
  }

  override def skippedEntity(name: String) {
    text.append('&')
    text.append(name)
    text.append(';')
  }

  override def fatalError(expection: SAXParseException) {
    // no op
  }
}

class FormulaIndexWriter(dir: Directory) {

  protected val writer = {
    val analyzer = new FormulaAnalyzer
    val mergePolicy = new LogByteSizeMergePolicy
    mergePolicy.setUseCompoundFile(false)
    val iwc = new IndexWriterConfig(Version.LUCENE_36, analyzer)
      .setOpenMode(OpenMode.CREATE)
      .setRAMBufferSizeMB(Settings.getDouble("index.ram_size"))
      .setSimilarity(new FormulaSimilarity)
      .setMergePolicy(mergePolicy)
    new IndexWriter(dir, iwc)
  }

  def add(page: WikiPage) {
    for (math <- page.mathes) {
      val formula = new FormulaDocument(math, page)
      writer.addDocument(formula.toDocument)
    }
  }

  def close {
    writer.forceMerge(1)
    writer.close()
  }
}

class PageIndexWriter(dir: Directory) extends FormulaIndexWriter(dir) {
  override def add(page: WikiPage) {
    writer.addDocument(new PageDocument(page).toDocument)
  }
}

/**
 * The term level is stored in the payload of the formula term.
 *
 */
class FormulaTermLevelPayloadFunction(val termLevelInQuery: Int) extends PayloadFunction {

  override def currentScore(docId: Int, field: String, start: Int, end: Int,
    numPayloadsSeen: Int, currentScore: Float, currentPayloadScore: Float): Float = {
    currentScore max (1.0F / (1.0F + (termLevelInQuery - currentPayloadScore).abs))
  }

  override def docScore(docId: Int, field: String, numPayloadsSeen: Int, payloadScore: Float): Float = payloadScore

  override def hashCode = termLevelInQuery

  override def equals(o: Any): Boolean = o match {
    case that: FormulaTermLevelPayloadFunction => this.termLevelInQuery == that.termLevelInQuery
    case _ => false
  }

  override def explain(docId: Int, field: String, numPayloadsSeen: Int,
    payloadScore: Float): Explanation = {
    val termLevel = new Explanation();
    termLevel.setDescription("term level in query");
    termLevel.setValue(termLevelInQuery);

    val result = super.explain(docId, field, numPayloadsSeen,
      payloadScore);
    result.addDetail(termLevel)
    result
  }
}

/**
 * Read the document of {@link TFIDFSimilarity} to know the meaning of every
 * method.
 */
class FormulaSimilarity extends DefaultSimilarity {
  //override def queryNorm(sumOfSquaredWeights: Float) = 1.0F

  override def scorePayload(doc: Int, start: Int, end: Int, payload: BytesRef): Float = {
    // decode the term level
    PayloadHelper.decodeInt(payload.bytes, payload.offset);
  }

  //override def coord(overlap: Int, maxOverlap: Int) = 1.0F

  override def computeNorm(state: FieldInvertState, norm: Norm) {
    val numTerms = state.getLength()
    norm.setByte(encodeNormValue(state.getBoost()
      * ((1.0 / numTerms).toFloat)));
  }

  override def tf(freq: Float) = 1.0F

  override def idf(docFreq: Long, numDocs: Long): Float = super.idf(docFreq, numDocs)

  override def sloppyFreq(distance: Int): Float = distance
}

class FormulaSearcher(dir: Directory) extends Logging {

  private val searcher = {
    logger.info(s"Opening index dir: ${dir}")
    val reader = DirectoryReader.open(dir)
    new IndexSearcher(reader)
  }
  private val similarity = new FormulaSimilarity
  similarity.setDiscountOverlaps(false)
  searcher.setSimilarity(similarity)

  def search(query: String): scala.collection.mutable.Map[Long, FormulaSearchResult] = {
    new FormulaQueryParser()
      .parse(query) match {
        case Some(q) =>
          val totalHits = getTotalHits(q)
          if (totalHits > 0) {
            val result = scala.collection.mutable.Map[Long, FormulaSearchResult]()
            for (scoreDoc <- searcher.search(q, totalHits).scoreDocs) {
              val doc = searcher.doc(scoreDoc.doc)
              val pageId = doc.get("doc_id").toLong
              if (!result.contains(pageId)) {
                result += pageId -> FormulaSearchResult(scoreDoc.doc, doc.get("formula_id").toLong, pageId, scoreDoc.score)
              } else {
                // skip this formula since we have already included the page
              }
            }
            result
          } else {
            scala.collection.mutable.Map[Long, FormulaSearchResult]()
          }
        case None => scala.collection.mutable.Map[Long, FormulaSearchResult]()
      }
  }

  private def getTotalHits(query: Query) = {
    searcher.search(query, 1).totalHits
  }

  def doc(docId: Int) = searcher.doc(docId)

  def explain(query: String, docId: Int) = {
    new FormulaQueryParser().parse(query) map { searcher.explain(_, docId) }
  }

  def explainQuery(query: String) = {
    new FormulaQueryParser().explain(query)
  }
}

class PageSearcher(dir: Directory) extends Logging {

  private val searcher = {
    logger.info(s"Opening index dir: ${dir}")
    val reader = DirectoryReader.open(dir)
    new IndexSearcher(reader)
  }
  private val similarity = new FormulaSimilarity
  similarity.setDiscountOverlaps(false)
  searcher.setSimilarity(similarity)

  def search(query: String): scala.collection.mutable.Map[Long, PageSearchResult] = {
    new FormulaQueryParser()
      .parse(query) match {
        case Some(q) =>
          val totalHits = getTotalHits(q)
          if (totalHits > 0) {
            val result = scala.collection.mutable.Map[Long, PageSearchResult]()
            searcher.search(q, totalHits).scoreDocs foreach { scoreDoc =>
              val doc = searcher.doc(scoreDoc.doc)
              val pageId = doc.get("doc_id").toLong
              result += pageId -> PageSearchResult(scoreDoc.doc, pageId, scoreDoc.score)
            }
            result
          } else {
            scala.collection.mutable.Map[Long, PageSearchResult]()
          }
        case None => scala.collection.mutable.Map[Long, PageSearchResult]()
      }
  }

  private def getTotalHits(query: Query) = {
    searcher.search(query, 1).totalHits
  }

  def doc(docId: Int) = searcher.doc(docId)

  def explain(query: String, docId: Int) = {
    new FormulaQueryParser().parse(query) map { searcher.explain(_, docId) }
  }
}

object FormulaSearcher extends Logging {

  val formulaSearcher = {
    val dir = new RAMDirectory(new MMapDirectory(new File(Settings.getString("index.formula_dir"))), IOContext.READ)
    new FormulaSearcher(dir)
  }

  val pageSearcher = {
    val dir = new RAMDirectory(new MMapDirectory(new File(Settings.getString("index.page_dir"))), IOContext.READ)
    new PageSearcher(dir)
  }

  /**
   * @param query
   * @param page the page id. Start from 1.
   * @param sizeOfPage the size of every page. Must greater than 0.
   * @return
   */
  def search(query: String, page: Int, pageSize: Int) = {
    if (page <= 0) {
      throw new IllegalArgumentException("page <= 0")
    }

    if (pageSize <= 0) {
      throw new IllegalArgumentException("sizeOfPage <= 0")
    }

    val beginTime = System.currentTimeMillis()

    logger.info("Search page")
    val pageResults = pageSearcher.search(query)
    logger.info("Search formula")
    val formulaResults = formulaSearcher.search(query)
    logger.info("Merge")
    require(formulaResults.size == pageResults.size, s"Not equal: ${formulaResults.size} ${pageResults.size}")

    val results = scala.collection.mutable.ListBuffer[FinalResult]()
    formulaResults foreach {
      case (pageId, formula) => {
        require(pageResults.contains(pageId))
        //        if (pageResults.contains(pageId)) {
        results += new FinalResult(formula, pageResults(pageId))
        //        }
      }
    }

    val endTime = System.currentTimeMillis()
    logger.info("Done")
    val numTotalHits = results.size
    val start = (page - 1) * pageSize
    val end = (start + pageSize) min numTotalHits
    val sortedResults = results.toList.sortWith(_.score > _.score).slice(start, end)
    val mathml = new LatexToMathml().toMathml(query);
    if (numTotalHits > 0) {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> {
          formulaSearcher.explainQuery(query) match {
            case Some(e) => e
            case None => ""
          }
        },
        "results" -> (sortedResults map {
          _.toJson(
            formulaSearcher,
            pageSearcher,
            query)
        }),
        "page" -> page,
        "pageSize" -> pageSize,
        "total" -> numTotalHits,
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    } else {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> {
          formulaSearcher.explainQuery(query) match {
            case Some(e) => e
            case None => ""
          }
        },
        "results" -> Json.arr(),
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    }
  }
}

case class FormulaSearchResult(docId: Int, formulaId: Long, pageId: Long, score: Float)
case class PageSearchResult(docId: Int, pageId: Long, score: Float)

case class FinalResult(formula: FormulaSearchResult, page: PageSearchResult) extends Ordered[FinalResult] {
  require(formula.pageId == page.pageId)

  import FinalResult._
  val score = formula.score * formulaWeight + page.score * pageWeight

  def toJson(
    formulaSearcher: FormulaSearcher,
    pageSearcher: PageSearcher,
    query: String) = {
    val formulaDoc = formulaSearcher.doc(formula.docId)
    val pageDoc = pageSearcher.doc(page.docId)
    val pageTitle = pageDoc.get("doc_title")
    Json.obj(
      "doc" ->
        Json.obj(
          "formula_id" -> formula.formulaId,
          "formula" -> formulaDoc.get("formula"),
          "doc_id" -> page.pageId,
          "doc_title" -> pageTitle,
          "doc_url" -> {
            "http://en.wikipedia.org/wiki/" + URLEncoder.encode(pageTitle.replaceAll(" ", "_"), "UTF-8")
          }),
      "score" -> score,
      "explain" -> (
        "<div>"
        + (formulaSearcher.explain(query, formula.docId) map (_.toHtml) getOrElse "")
        + "</div>"
        + "<div style='background-color:gray'>"
        + (pageSearcher.explain(query, page.docId) map (_.toHtml) getOrElse "")
        + "</div>"))
  }

  override def compare(that: FinalResult): Int = {
    if (this.score < that.score) {
      1
    } else if (this.score > that.score) {
      -1
    } else {
      0
    }
  }
}

object FinalResult {
  val formulaWeight = Settings.getDouble("webserver.formula_weight")
  val pageWeight = Settings.getDouble("webserver.page_weight")
}

