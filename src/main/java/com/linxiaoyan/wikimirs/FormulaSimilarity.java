package com.linxiaoyan.wikimirs;

import org.apache.lucene.analysis.payloads.PayloadHelper;
import org.apache.lucene.index.FieldInvertState;
import org.apache.lucene.index.Norm;
import org.apache.lucene.search.similarities.DefaultSimilarity;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.util.BytesRef;

/**
 * Read the document of {@link TFIDFSimilarity} to know the meaning of every
 * method.
 */
public class FormulaSimilarity extends DefaultSimilarity {

	@Override
	public float queryNorm(float sumOfSquaredWeights) {
		return 1.0F;
	}

	@Override
	public float scorePayload(int doc, int start, int end, BytesRef payload) {
		// decode the term level
		return PayloadHelper.decodeInt(payload.bytes, payload.offset);
	}

	@Override
	public float coord(int overlap, int maxOverlap) {
		return 1.0F;
	}

	@Override
	public void computeNorm(FieldInvertState state, Norm norm) {
		super.computeNorm(state, norm);
	}

	@Override
	public float tf(float freq) {
		return super.tf(freq);
	}

	@Override
	public float idf(long docFreq, long numDocs) {
		return super.idf(docFreq, numDocs);
	}

}
