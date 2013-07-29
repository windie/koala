package com.linxiaoyan.wikimirs;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.lucene.analysis.payloads.PayloadHelper;
import org.apache.lucene.index.Term;
import org.apache.lucene.util.BytesRef;

public class FormulaTerm {

	private String term;
	private int level;
	private boolean generalization;

	public FormulaTerm(String term, int level, boolean generalization) {
		this.term = term;
		this.level = level;
		this.generalization = generalization;
	}

	private float getNormalizeScore() {
		return generalization ? 0.5F : 1F;
	}

	public String getTerm() {
		return term;
	}

	public int getLevel() {
		return level;
	}

	public boolean getGeneralization() {
		return generalization;
	}

	public BytesRef toPayload() {
		return new BytesRef(PayloadHelper.encodeInt(level));
	}

	public TermLevelTermQuery toTermQuery() {
		TermLevelTermQuery termQuery = new TermLevelTermQuery(new Term("formula",
				term), new FormulaTermLevelPayloadFunction(level));
		termQuery.setBoost(getNormalizeScore());
		return termQuery;
	}

	public String explain() {
		String escapedTerm = StringEscapeUtils.escapeHtml4(term);
		return "term: <code class='xml'>" + escapedTerm + "</code>, "
				+ "level: <code>" + level + "</code>, "
				+ "generalization: <code>" + generalization + "</code>";
	}

	@Override
	public String toString() {
		return "FormulaTerm [term=" + term + ", level=" + level
				+ ", generalization=" + generalization + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (generalization ? 1231 : 1237);
		result = prime * result + level;
		result = prime * result + ((term == null) ? 0 : term.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FormulaTerm other = (FormulaTerm) obj;
		if (generalization != other.generalization)
			return false;
		if (level != other.level)
			return false;
		if (term == null) {
			if (other.term != null)
				return false;
		} else if (!term.equals(other.term))
			return false;
		return true;
	}

}
