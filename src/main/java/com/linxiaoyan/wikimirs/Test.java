package com.linxiaoyan.wikimirs;

import org.apache.lucene.util.SmallFloat;

public class Test {
	public static void main(String[] args) {
		System.out.println(SmallFloat.byte315ToFloat(SmallFloat
				.floatToByte315(0.5F)));
	}
}
