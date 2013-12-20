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
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.FieldInvertState
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.Norm
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Explanation
import org.apache.lucene.search.IndexSearcher
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
import org.xml.sax.Attributes
import org.xml.sax.InputSource
import org.xml.sax.SAXParseException

import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import uk.ac.ed.ph.snuggletex.SerializationMethod
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.XMLStringOutputOptions

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

//case class Tag(val parent: Tag, val label: String) {
//  val children = ArrayBuffer[Tag]()
//
//  def add(node: Tag) {
//    children += node
//  }
//
//  def isText = false
//
//  def isOnlyText = {
//    children.isEmpty || children.forall(child => {
//      child.label == "mo" || child.label == "mi" || child.isText
//    })
//  }
//
//  def toLabelString = "<" + label + "></" + label + ">"
//
//  override def toString = "<" + label + ">" + children.map(_.toString).mkString + "</" + label + ">"
//}
//
//class Text(parent: Tag, label: String) extends Tag(parent, label) {
//  override def isText = true
//  override def toString = label
//}

class MathmlParser {
  private val handler = new MathmlXMLHandler

  private val tokenizer = new MathmlTokenizer

  private val parser = new SAXParser
  parser.setFeature("http://xml.org/sax/features/validation", false);
  parser.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
  parser.setFeature("http://xml.org/sax/features/namespaces", false)
  parser.setFeature("http://apache.org/xml/features/validation/unparsed-entity-checking", false)
  parser.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)
  parser.setContentHandler(handler)
  parser.setErrorHandler(handler)

  def parse(mathml: String, tokens: ListBuffer[FormulaTerm]) {
    handler.clear
    parser.parse(new InputSource(new StringReader(mathml)))
    tokenizer.toTokens(handler.node.iterator.next, tokens)
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

  private val parser = new MathmlParser

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
    val latex = IOUtils.toString(input)
    try {
      val mathml = latex2mathml.toMathml(latex)
      tokens.clear
      parser.parse(mathml, tokens)
    } catch {
      case e: Throwable => {
        println("find error: latex")
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

  var node: MathmlTag = new MathmlTag(null, "dummy")
  var text = new java.lang.StringBuilder

  def getTree = node.iterator.next

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
        node.addChild(new MathmlText(node, trim))
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

  private val writer = {
    val analyzer = new FormulaAnalyzer
    val iwc = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    iwc.setOpenMode(OpenMode.CREATE)
    iwc.setRAMBufferSizeMB(Settings.getDouble("index.ram_size"))
    iwc.setSimilarity(new FormulaSimilarity)
    new IndexWriter(dir, iwc)
  }

  def add(formulaDoc: FormulaDocument) {
    writer.addDocument(formulaDoc.toDocument)
  }

  def close {
    writer.forceMerge(1)
    writer.close()
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

class FormulaSearcher(dir: Directory) {

  private val searcher = {
    val reader = DirectoryReader.open(dir)
    new IndexSearcher(reader)
  }
  private val similarity = new FormulaSimilarity
  similarity.setDiscountOverlaps(false)
  searcher.setSimilarity(similarity)

  def search(query: String, sizeOfResult: Int) = {
    new FormulaQueryParser().parse(query) map { searcher.search(_, sizeOfResult) }
  }

  def explain(query: String, docId: Int) = {
    new FormulaQueryParser().parse(query) map { searcher.explain(_, docId) }
  }

  def explainQuery(query: String) = {
    new FormulaQueryParser().explain(query)
  }

  def doc(docId: Int) = searcher.doc(docId)
}

object FormulaSearcher {

  val searcher = {
    val dir = FSDirectory.open(new File(Settings.getString("index.dir")))
    new FormulaSearcher(dir)
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

    val results = searcher.search(query, page * pageSize)

    val endTime = System.currentTimeMillis()

    val mathml = new LatexToMathml().toMathml(query);

    results match {
      case Some(topDocs) => {
        val hits = topDocs.scoreDocs
        val numTotalHits = topDocs.totalHits

        val start = (page - 1) * pageSize
        val end = (start + pageSize) min numTotalHits
        val resultsJson = for (i <- start until end) yield {
          scoreDocToJson(query, hits(i))
        }

        Json.obj(
          "status" -> "OK",
          "query_mathml" -> mathml,
          "query_detail" -> {
            searcher.explainQuery(query) match {
              case Some(e) => e
              case None => ""
            }
          },
          "results" -> resultsJson,
          "page" -> page,
          "pageSize" -> pageSize,
          "total" -> numTotalHits,
          "time" -> ((endTime - beginTime) / 1000.0).toString)
      }
      case None => {
        Json.obj(
          "status" -> "OK",
          "query_mathml" -> mathml,
          "query_detail" -> {
            searcher.explainQuery(query) match {
              case Some(e) => e
              case None => ""
            }
          },
          "results" -> Json.arr())
      }
    }
  }

  private def scoreDocToJson(query: String, scoreDoc: ScoreDoc) = {
    val docId = scoreDoc.doc
    val doc = searcher.doc(docId)
    Json.obj(
      "doc" -> documentToJson(doc),
      "score" -> scoreDoc.score,
      "explain" -> (searcher.explain(query, docId) match {
        case Some(e) => e.toHtml
        case None => ""
      }))
  }

  private def documentToJson(doc: Document) = {
    Json.obj(
      "formula_id" -> doc.get("formula_id").toLong,
      "formula" -> doc.get("formula"),
      "doc_id" -> doc.get("doc_id").toLong,
      "doc_title" -> doc.get("doc_title"),
      "doc_url" -> {
        "http://en.wikipedia.org/wiki/" + URLEncoder.encode(doc.get("doc_title").replaceAll(" ", "_"), "UTF-8")
      })
  }

}
