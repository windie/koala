package com.linxiaoyan.wikimirs.lab

import com.linxiaoyan.wikimirs.{ MathmlNode, MathmlTag, MI, MO, MN, NaiveNode, Text, LatexToMathml, AdjustTreeVisitor, MathmlBuilder, ErrorCount }
import com.linxiaoyan.wikimirs.{ Settings, WikiPage, PageDocument }
import scala.collection.mutable.ListBuffer
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

class LabTermLevelPayloadFunction extends PayloadFunction {

  override def currentScore(docId: Int, field: String, start: Int, end: Int,
    numPayloadsSeen: Int, currentScore: Float, currentPayloadScore: Float): Float = {
    currentScore + currentPayloadScore
  }

  override def docScore(docId: Int, field: String, numPayloadsSeen: Int, payloadScore: Float): Float = payloadScore / numPayloadsSeen

  override def hashCode = 0

  override def equals(o: Any): Boolean = o match {
    case that: LabTermLevelPayloadFunction => o.isInstanceOf[LabTermLevelPayloadFunction]
    case _ => false
  }
}

case class LabTerm(val term: String, val level: Int, val punishment: Double) {
  import scala.math._
  def toPayload(n: Int): BytesRef = {
    val weight = pow(CompositeTokenizer.l, level) * punishment / n
    new BytesRef(PayloadHelper.encodeFloat(weight.toFloat))
  }
}

case class LabQueryTerm(val term: String) {

  def toTermQuery(): LabLevelTermQuery = {
    val termQuery = new LabLevelTermQuery(new Term("formula",
      term), new LabTermLevelPayloadFunction)
    termQuery
  }

  def explain: String = {
    val escapedTerm = StringEscapeUtils.escapeHtml4(term);
    s"""term: <code class='xml'>$escapedTerm</code>""";
  }
}

class Eliminator {
  def visit(node: MathmlNode): MathmlNode = node
}

class MIEliminator extends Eliminator {
  override def visit(node: MathmlNode): MathmlNode = node match {
    case tagNode: MathmlTag => {
      val newChildren = ListBuffer[MathmlNode]()
      for (child <- tagNode.children) {
        newChildren += visit(child)
      }
      new MathmlTag(tagNode.parent, tagNode.tag, newChildren)
    }
    case mi: MI => new MI("")
    case _ => node
  }
}

class MNEliminator extends Eliminator {
  override def visit(node: MathmlNode): MathmlNode = node match {
    case tagNode: MathmlTag => {
      val newChildren = ListBuffer[MathmlNode]()
      for (child <- tagNode.children) {
        newChildren += visit(child)
      }
      new MathmlTag(tagNode.parent, tagNode.tag, newChildren)
    }
    case mn: MN => new MN("")
    case _ => node
  }
}

class MIMNEliminator extends Eliminator {
  override def visit(node: MathmlNode): MathmlNode = node match {
    case tagNode: MathmlTag => {
      val newChildren = ListBuffer[MathmlNode]()
      for (child <- tagNode.children) {
        newChildren += visit(child)
      }
      new MathmlTag(tagNode.parent, tagNode.tag, newChildren)
    }
    case mn: MN => new MN("")
    case mi: MI => new MI("")
    case _ => node
  }
}

class SubtreeTokenizer(val punishment: Double) {

  def toTokens(node: MathmlNode, tokens: ListBuffer[LabTerm]) {
    toTokens(node, 1, tokens);
  }

  private def toTokens(node: MathmlNode, level: Int, tokens: ListBuffer[LabTerm]): Unit = node match {
    case node: NaiveNode => {
      val text = node.text
      if (text.length > 1) {
        tokens += new LabTerm(node.toString, level, punishment)
      }
    }
    case node: Text => {
      if (node.text.length <= 1) {
        return
      }
      tokens += new LabTerm(node.toString, level, punishment)
    }
    case node: MO => {
      tokens += new LabTerm(node.toString, level, punishment)
      node.children.foreach {
        toTokens(_, level + 1, tokens);
      }
    }
    case node: MathmlTag => {
      var nonGeneralized = getChildrenString(node);
      var skipThisNode = false;
      if (node.tag.equals("math")
        || node.tag.equals("mrow")) {
        if (node.children.size == 1) {
          skipThisNode = true
        }
      } else {
        nonGeneralized = node.tagString(nonGeneralized)
      }
      if (!skipThisNode) {
        tokens += new LabTerm(nonGeneralized, level, punishment);
      }
      node.children.foreach {
        toTokens(_, level + 1, tokens);
      }
    }
  }

  private def getChildrenString(node: MathmlNode) = {
    node.children.map(_.toString).mkString
  }
}

class CompositeTokenizer {
  import CompositeTokenizer._
  private val eliminators = List(
    (new Eliminator, new SubtreeTokenizer(1.0 + v + c + vc)),
    (new MIEliminator, new SubtreeTokenizer(v + vc)),
    (new MNEliminator, new SubtreeTokenizer(c + vc)),
    (new MIMNEliminator, new SubtreeTokenizer(vc)))

  def toTokens(node: MathmlNode, tokens: ListBuffer[LabTerm]) {
    eliminators foreach {
      case (eliminator, tokenizer) => {
        tokenizer.toTokens(eliminator.visit(node), tokens)
      }
    }
  }
}

object CompositeTokenizer {
  val l = 0.7
  val v = 0.8
  val c = 0.5
  val vc = v * c
}

class LabTokenizer(_input: Reader) extends Tokenizer(_input) {

  private val latex2mathml = new LatexToMathml

  private val termAtt: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  private val payloadAtt: PayloadAttribute = addAttribute(classOf[PayloadAttribute])

  private val tokenizer = new CompositeTokenizer
  private val visitor = new AdjustTreeVisitor
  private val builder = new MathmlBuilder

  private var tokens = ListBuffer[LabTerm]()
  private var nodeSize = 0

  override def incrementToken = {
    if (tokens.isEmpty) {
      false
    } else {
      val formulaTerm = tokens.head
      termAtt.setEmpty().append(formulaTerm.term)
      payloadAtt.setPayload(formulaTerm.toPayload(nodeSize))
      tokens = tokens.tail
      true
    }
  }

  def nodeCount(node: MathmlNode): Int = node match {
    case tag: MathmlTag => {
      tag.children.map { nodeCount(_) }.sum + 1
    }
    case _ => 1
  }

  override def reset {
    val latexes = IOUtils.toString(input)
    tokens.clear
    latexes.split('\0').filterNot(_.isEmpty).foreach { latex =>
      try {
        val mathml = latex2mathml.toMathml(latex)
        val node = visitor.visit(builder.parse(mathml))
        nodeSize = nodeCount(node)
        tokenizer.toTokens(node, tokens)
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

class LabAnalyzer extends Analyzer {

  override def createComponents(fieldName: String,
    reader: Reader): TokenStreamComponents = {
    val tokenizer = new LabTokenizer(reader)
    new TokenStreamComponents(tokenizer, tokenizer) {
      override def setReader(reader: Reader) {
        super.setReader(reader);
      }
    };
  }
}

/**
 * Read the document of {@link TFIDFSimilarity} to know the meaning of every
 * method.
 */
class LabSimilarity extends DefaultSimilarity {
  //override def queryNorm(sumOfSquaredWeights: Float) = 1.0F

  override def scorePayload(doc: Int, start: Int, end: Int, payload: BytesRef): Float = {
    PayloadHelper.decodeFloat(payload.bytes, payload.offset);
  }

  override def computeNorm(state: FieldInvertState, norm: Norm) {
    val numTerms = state.getLength()
    norm.setByte(encodeNormValue(state.getBoost()
      * ((1.0 / numTerms).toFloat)));
  }
  //
  //  override def coord(overlap: Int, maxOverlap: Int): Float = {
  //    overlap * overlap / maxOverlap.toFloat;
  //  }

  override def tf(freq: Float) = 1.0F

  override def idf(docFreq: Long, numDocs: Long): Float = super.idf(docFreq, numDocs)

  override def sloppyFreq(distance: Int): Float = distance
}

class LabIndexWriter(dir: Directory) {

  protected val writer = {
    val analyzer = new LabAnalyzer
    val mergePolicy = new LogByteSizeMergePolicy
    mergePolicy.setUseCompoundFile(false)
    val iwc = new IndexWriterConfig(Version.LUCENE_36, analyzer)
      .setOpenMode(OpenMode.CREATE)
      .setRAMBufferSizeMB(Settings.getDouble("index.ram_size"))
      .setSimilarity(new LabSimilarity)
      .setMergePolicy(mergePolicy)
    new IndexWriter(dir, iwc)
  }

  def add(page: WikiPage) {
    writer.addDocument(new PageDocument(page).toDocument)
  }

  def close {
    writer.forceMerge(1)
    writer.close()
  }
}

class LabLevelTermQuery(term: Term, function: PayloadFunction) extends PayloadTermQuery(term, function) {

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

class LabQueryParser {

  private val analyzer = new LabAnalyzer

  def parse(query: String): Option[Query] = {
    val source = analyzer.tokenStream("formula", new StringReader(query));
    source.reset()
    val combinedQuery = new BooleanQuery(false)
    var numTokens = 0

    val termAtt: CharTermAttribute = source.getAttribute(classOf[CharTermAttribute])
    while (source.incrementToken()) {
      numTokens += 1
      val formulaTerm = new LabQueryTerm(termAtt.toString)
      combinedQuery.add(formulaTerm.toTermQuery, BooleanClause.Occur.SHOULD)
    }

    if (numTokens == 0)
      return None

    Some(combinedQuery)
  }

  def explain(query: String): Option[String] = {
    val source = analyzer.tokenStream("formula", new StringReader(query));
    source.reset()
    val combinedQuery = new BooleanQuery(false)
    var numTokens = 0

    val termAtt: CharTermAttribute = source.getAttribute(classOf[CharTermAttribute])

    val explain = new StringBuilder
    explain ++= "<ul>"
    while (source.incrementToken()) {
      numTokens += 1
      val formulaTerm = new LabQueryTerm(termAtt.toString)
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

class LabSearcher(dir: Directory) extends Logging {

  private val searcher = {
    logger.info(s"Opening index dir: ${dir}")
    val reader = DirectoryReader.open(dir)
    new IndexSearcher(reader)
  }
  private val similarity = new LabSimilarity
  similarity.setDiscountOverlaps(false)
  searcher.setSimilarity(similarity)

  def search(query: String, size: Int): Option[TopDocs] = {
    new LabQueryParser()
      .parse(query) map { searcher.search(_, size) }
  }

  private def getTotalHits(query: Query) = {
    searcher.search(query, 1).totalHits
  }

  def doc(docId: Int) = searcher.doc(docId)

  def explain(query: String, docId: Int) = {
    new LabQueryParser().parse(query) map { searcher.explain(_, docId) }
  }

  def explainQuery(query: String) = {
    new LabQueryParser().explain(query)
  }
}

object LabSearcher extends Logging {

  val searcher = {
    val dir = new RAMDirectory(new MMapDirectory(new File(Settings.getString("index.page_dir"))), IOContext.READ)
    new LabSearcher(dir)
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

    val topDocs = searcher.search(query, page * pageSize)
    val results = topDocs match {
      case Some(r) => {
        val start = (page - 1) * pageSize
        val end = (start + pageSize) min r.totalHits
        r.scoreDocs.slice(start, end).toList
      }
      case None => Nil
    }

    val endTime = System.currentTimeMillis()

    val mathml = new LatexToMathml().toMathml(query);
    if (results.size > 0) {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> {
          searcher.explainQuery(query) match {
            case Some(e) => e
            case None => ""
          }
        },
        "results" -> (results map { doc =>
          toJson(query, doc)
        }),
        "page" -> page,
        "pageSize" -> pageSize,
        "total" -> topDocs.get.totalHits,
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    } else {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> {
          searcher.explainQuery(query) match {
            case Some(e) => e
            case None => ""
          }
        },
        "results" -> Json.arr(),
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    }
  }

  def toJson(query: String, scoreDoc: ScoreDoc) = {
    val doc = searcher.doc(scoreDoc.doc)
    val pageTitle = doc.get("doc_title")
    //        doc.add(new TextField("formula", content, Field.Store.NO))
    Json.obj(
      "doc" ->
        Json.obj(
          "formula_id" -> 0,
          "formula" -> "",
          "doc_id" -> doc.get("doc_id"),
          "doc_title" -> pageTitle,
          "doc_url" -> {
            "http://en.wikipedia.org/wiki/" + URLEncoder.encode(pageTitle.replaceAll(" ", "_"), "UTF-8")
          }),
      "score" -> scoreDoc.score,
      "explain" -> (
        "" +
        (searcher.explain(query, scoreDoc.doc) map (_.toHtml) getOrElse "")))
  }
}
