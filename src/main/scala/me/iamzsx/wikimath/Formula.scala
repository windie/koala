package me.iamzsx.wikimath

import org.apache.lucene.analysis.payloads.PayloadHelper
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.payloads.PayloadTermQuery
import org.apache.lucene.util.BytesRef
import org.apache.lucene.index.Term
import me.iamzsx.xyz.TermLevelPayloadFunction
import org.apache.lucene.search.BooleanClause

class FormulaTerm(
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

