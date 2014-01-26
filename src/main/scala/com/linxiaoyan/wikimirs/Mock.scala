package com.linxiaoyan.wikimirs.mock

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
import com.linxiaoyan.wikimirs.LatexToMathml
import com.linxiaoyan.wikimirs.FormulaDocument

case class MockResult(val formula: String, val title: String) {

  def toJson = {
    Json.obj(
      "doc" ->
        Json.obj(
          "formula_id" -> FormulaDocument.ID.incrementAndGet(),
          "formula" -> formula,
          "doc_id" -> 0,
          "doc_title" -> title,
          "doc_url" -> {
            "http://en.wikipedia.org/wiki/" + URLEncoder.encode(title.replaceAll(" ", "_"), "UTF-8")
          }),
      "score" -> 0,
      "explain" -> "")
  }
}

object MockSearch extends Logging {

  val mockResults = {
    val mockResults = scala.collection.mutable.Map[String, List[MockResult]]()
    val iter = scala.io.Source.fromFile("result.txt")("UTF-8").getLines
    while (iter.hasNext) {
      val query = iter.next
      println(query)
      val result = ListBuffer[MockResult]()
      require(iter.hasNext)
      var formula = iter.next
      while (!formula.isEmpty) {
        require(iter.hasNext)
        val url = iter.next
        require(iter.hasNext)
        val title = iter.next
        result += MockResult(formula, title)
        formula = iter.next
      }
      mockResults += query -> result.toList
    }
    mockResults.toMap
  }
  
  def search(query: String, page: Int, pageSize: Int) = {
    if (page <= 0) {
      throw new IllegalArgumentException("page <= 0")
    }

    if (pageSize <= 0) {
      throw new IllegalArgumentException("sizeOfPage <= 0")
    }

    val beginTime = System.currentTimeMillis()

    val results = mockResults.get(query) match {
      case Some(rs) => rs
      case None => Nil
    }

    val endTime = System.currentTimeMillis()

    val mathml = new LatexToMathml().toMathml(query);
    if (results.size > 0) {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> "",
        "results" -> (results map {
          _.toJson
        }),
        "page" -> page,
        "pageSize" -> pageSize,
        "total" -> results.size,
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    } else {
      Json.obj(
        "status" -> "OK",
        "query_mathml" -> mathml,
        "query_detail" -> "",
        "results" -> Json.arr(),
        "time" -> ((endTime - beginTime) / 1000.0).toString)
    }
  }
}