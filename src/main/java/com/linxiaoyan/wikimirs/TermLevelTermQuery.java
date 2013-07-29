package com.linxiaoyan.wikimirs;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.payloads.PayloadFunction;
import org.apache.lucene.search.payloads.PayloadTermQuery;
import org.apache.lucene.util.ToStringUtils;

public class TermLevelTermQuery extends PayloadTermQuery {

	public TermLevelTermQuery(Term term, PayloadFunction function) {
		super(term, function);
	}

	@Override
	public String toString(String field) {
		StringBuilder buffer = new StringBuilder();
		if (!term.field().equals(field))
			buffer.append(term.field() + ":");
		buffer.append("<code>" + StringEscapeUtils.escapeHtml4(term.text())
				+ "</code>");
		buffer.append(ToStringUtils.boost(getBoost()));
		return buffer.toString();
	}
}
