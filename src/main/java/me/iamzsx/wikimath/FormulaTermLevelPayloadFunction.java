package me.iamzsx.wikimath;

import org.apache.lucene.search.Explanation;
import org.apache.lucene.search.payloads.PayloadFunction;

/**
 * The term level is stored in the payload of the formula term.
 * 
 */
public class FormulaTermLevelPayloadFunction extends PayloadFunction {

	private final int termLevelInQuery;

	public FormulaTermLevelPayloadFunction(int termLevelInQuery) {
		this.termLevelInQuery = termLevelInQuery;
	}

	@Override
	public float currentScore(int docId, String field, int start, int end,
			int numPayloadsSeen, float currentScore, float currentPayloadScore) {
		return currentScore + 1.0F
				/ (1.0F + Math.abs(termLevelInQuery - currentPayloadScore));
	}

	@Override
	public float docScore(int docId, String field, int numPayloadsSeen,
			float payloadScore) {
		return payloadScore;
	}

	@Override
	public int hashCode() {
		return termLevelInQuery;
	}

	@Override
	public boolean equals(Object o) {
		if (o == null)
			return false;
		if (!(o instanceof FormulaTermLevelPayloadFunction)) {
			return false;
		}
		FormulaTermLevelPayloadFunction that = (FormulaTermLevelPayloadFunction) o;
		return this.termLevelInQuery == that.termLevelInQuery;
	}

	@Override
	public Explanation explain(int docId, String field, int numPayloadsSeen,
			float payloadScore) {
		Explanation termLevel = new Explanation();
		termLevel.setDescription("term level in query");
		termLevel.setValue(termLevelInQuery);

		Explanation result = super.explain(docId, field, numPayloadsSeen,
				payloadScore);
		result.addDetail(termLevel);
		return result;
	}
}
