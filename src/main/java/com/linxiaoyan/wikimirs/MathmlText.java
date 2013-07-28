package com.linxiaoyan.wikimirs;

import java.util.ArrayList;
import java.util.Iterator;

public class MathmlText implements MathmlNode {

	private MathmlTag parent;

	private String text;

	public MathmlText(MathmlTag parent, String text) {
		this.parent = parent;
		this.text = text;
	}

	@Override
	public MathmlTag getParent() {
		return parent;
	}

	@Override
	public boolean isText() {
		return true;
	}

	@Override
	public Iterator<MathmlNode> iterator() {
		return new ArrayList<MathmlNode>().iterator();
	}

	@Override
	public String getLabel() {
		return text;
	}

	@Override
	public boolean isLeaf() {
		return true;
	}

	@Override
	public String toString() {
		return text;
	}
}
