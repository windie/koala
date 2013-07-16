package me.iamzsx.wikimath

import java.io.File
import java.io.IOException
import java.io.Reader
import java.io.StringReader

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import org.apache.commons.io.IOUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.tokenattributes.PayloadAttribute
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Query
import org.apache.lucene.search.payloads.PayloadTermQuery
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.BytesRef
import org.apache.lucene.util.Version

import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import me.iamzsx.xyz.TermLevelPayloadFunction
import me.iamzsx.xyz.TermLevelPayloadSimilarity
import play.api.libs.json.JsNull
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import uk.ac.ed.ph.snuggletex.SerializationMethod
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.XMLStringOutputOptions

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

class FormulaIndexWriter(dir: Directory) {

  private val writer = {
    val analyzer = new FormulaAnalyzer
    val iwc = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    iwc.setOpenMode(OpenMode.CREATE)
    iwc.setRAMBufferSizeMB(Config.get.getDouble("index.ram_size"));
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

class FormulaSearcher(dir: Directory) {

  private val searcher = {
    val reader = DirectoryReader.open(dir)
    new IndexSearcher(reader)
  }
  searcher.setSimilarity(new TermLevelPayloadSimilarity)

  def search(query: String, sizeOfResult: Int) = {
    searcher.search(new FormulaQueryParser().parse(query), sizeOfResult)
  }

  def explain(query: String, docId: Int) = {
    searcher.explain(new FormulaQueryParser().parse(query), docId)
  }

  def doc(docId: Int) = searcher.doc(docId)

}

object FormulaSearcher {

  val searcher = {
    val dir = FSDirectory.open(new File(Config.get.getString("index.dir")))
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

    val hits = results.scoreDocs
    val numTotalHits = results.totalHits

    val start = (page - 1) * pageSize
    val resultsJson = for (i <- start until numTotalHits) yield {
      val docId = hits(i).doc
      val doc = searcher.doc(docId)
      Json.obj(
        "doc" -> documentToJson(doc),
        "score" -> hits(i).score,
        "explain" -> searcher.explain(query, docId).toHtml)
    }

    Json.obj(
      "status" -> "OK",
      "results" -> resultsJson,
      "page" -> page,
      "pageSize" -> pageSize,
      "total" -> numTotalHits,
      "time" -> ((endTime - beginTime) / 1000.0).toString)
  }

  def documentToJson(doc: Document) = {
    Json.obj(
      "formula_id" -> doc.get("formula_id").toLong,
      "formula" -> doc.get("formula"),
      "doc_id" -> doc.get("doc_id").toLong,
      "doc_title" -> doc.get("doc_title"),
      "doc_url" -> doc.get("doc_url")) // TODO How to get the url
  }

}


