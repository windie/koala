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
    val combinedQuery = new BooleanQuery(true) // TODO true or false
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
    val combinedQuery = new BooleanQuery(true) // TODO true or false
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
  def visit(root: MathmlNode): MathmlNode = {
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
            val children = node.children.map(child => visit(child)).foldLeft(ListBuffer[MathmlNode]()) {
              case (children, child) => {
                if (children.isEmpty) {
                  children += child
                } else {
                  if (node.tag != "msup" && node.tag != "msub") {
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
            if (operators.isEmpty) {
              operators push mo
            } else {
              if (priority(prev, operators.top) > priority(prev, mo)) {
                operators push mo
              } else {
                stack push (operators.pop)
                operators push mo
              }
            }
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
      case "*" => 4
      case "/" => 4
      case "%" => 4 // TODO
      case "<" => 6
      case ">" => 6
      case "<=" => 6 // TODO
      case "&leq;" => 6
      case "&GreaterEqual;" => 6
      case ">=" => 6 // TODO
      case "==" => 7 // TODO
      case "!=" => 7 // TODO
      case "&&" => 8 // TODO
      case "||" => 9 // TODO
      case _ => 100
    }
  }
}

class ReorderOperantsVisitor extends TreeVisitor {
  def visit(node: MathmlNode): MathmlNode = node match {
    case operator: MO if (operator.text == "+" || operator.text == "*") &&
      operator.children.size == 2 &&
      operator.children(0) > operator.children(1) =>
      {
        val children = List(operator.children(1), operator.children(0))
        new MO(operator.text, children)
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

class LatexToMathml {
  private val engine = new SnuggleEngine

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
          println("find error: latex")
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
    currentScore + 1.0F / (1.0F + (termLevelInQuery - currentPayloadScore).abs);
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
    result.addDetail(termLevel);
    result;
  }
}

/**
 * Read the document of {@link TFIDFSimilarity} to know the meaning of every
 * method.
 */
class FormulaSimilarity extends DefaultSimilarity {
  override def queryNorm(sumOfSquaredWeights: Float) = 1.0F

  override def scorePayload(doc: Int, start: Int, end: Int, payload: BytesRef): Float = {
    // decode the term level
    PayloadHelper.decodeInt(payload.bytes, payload.offset);
  }

  override def coord(overlap: Int, maxOverlap: Int) = 1.0F

  override def computeNorm(state: FieldInvertState, norm: Norm) {
    val numTerms = state.getLength();
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
                result += pageId -> FormulaSearchResult(scoreDoc.doc, doc.get("formula_id").toLong, doc.get("formula"), pageId, scoreDoc.score)
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
              result += pageId -> PageSearchResult(scoreDoc.doc, pageId, doc.get("doc_title"), scoreDoc.score)
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

  def explain(query: String, docId: Int) = {
    new FormulaQueryParser().parse(query) map { searcher.explain(_, docId) }
  }
}

object FormulaSearcher {

  val formulaSearcher = {
    val dir = FSDirectory.open(new File(Settings.getString("index.formula_dir")))
    new FormulaSearcher(dir)
  }

  val pageSearcher = {
    val dir = FSDirectory.open(new File(Settings.getString("index.page_dir")))
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

    val pageResults = pageSearcher.search(query)
    val formulaResults = formulaSearcher.search(query)

    require(formulaResults.size == pageResults.size, s"Not equal: ${formulaResults.size} ${pageResults.size}")

    val results = scala.collection.mutable.TreeSet[FinalResult]()
    formulaResults foreach {
      case (pageId, formula) => {
        require(pageResults.contains(pageId))
        results += new FinalResult(formula, pageResults(pageId))
      }
    }

    val endTime = System.currentTimeMillis()

    val mathml = new LatexToMathml().toMathml(query);

    val numTotalHits = results.size
    val start = (page - 1) * pageSize
    val end = (start + pageSize) min numTotalHits
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
        "results" -> (results.slice(start, end) map {
          _.toJson(
            formulaSearcher,
            pageSearcher,
            query)
        } toList),
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
        "results" -> Json.arr())
    }
  }
}

case class FormulaSearchResult(docId: Int, formulaId: Long, formula: String, pageId: Long, score: Float)
case class PageSearchResult(docId: Int, pageId: Long, title: String, score: Float)

case class FinalResult(formula: FormulaSearchResult, page: PageSearchResult) extends Ordered[FinalResult] {
  require(formula.pageId == page.pageId)

  import FinalResult._
  val score = formula.score * formulaWeight + page.score * pageWeight

  def toJson(
    formulaSearcher: FormulaSearcher,
    pageSearcher: PageSearcher,
    query: String) = {
    Json.obj(
      "doc" ->
        Json.obj(
          "formula_id" -> formula.formulaId,
          "formula" -> formula.formula,
          "doc_id" -> page.pageId,
          "doc_title" -> page.title,
          "doc_url" -> {
            "http://en.wikipedia.org/wiki/" + URLEncoder.encode(page.title.replaceAll(" ", "_"), "UTF-8")
          }),
      "score" -> score,
      "explain" -> (
        (formulaSearcher.explain(query, formula.docId) map (_.toHtml) getOrElse "")
        + (pageSearcher.explain(query, page.docId) map (_.toHtml) getOrElse "")))
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
