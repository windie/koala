package me.iamzsx.xyz;

import org.apache.lucene.analysis.payloads.PayloadHelper;
import org.apache.lucene.search.similarities.DefaultSimilarity;
import org.apache.lucene.util.BytesRef;

public class TermLevelPayloadSimilarity extends DefaultSimilarity {

	@Override
	public float scorePayload(int doc, int start, int end, BytesRef payload) {
		System.out.println("TODO: "
				+ PayloadHelper.decodeInt(payload.bytes, payload.offset));
		return PayloadHelper.decodeInt(payload.bytes, payload.offset);
	}

}
