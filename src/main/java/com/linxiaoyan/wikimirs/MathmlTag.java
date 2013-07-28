package com.linxiaoyan.wikimirs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

public class MathmlTag implements MathmlNode {

	private MathmlTag parent;

	private String tag;

	private List<MathmlNode> children = new ArrayList<MathmlNode>();

	public MathmlTag(MathmlTag parent, String tag) {
		this.parent = parent;
		this.tag = tag;
	}

	@Override
	public MathmlTag getParent() {
		return parent;
	}

	@Override
	public boolean isText() {
		return false;
	}

	public void addChild(MathmlNode child) {
		children.add(child);
	}

	@Override
	public Iterator<MathmlNode> iterator() {
		return children.iterator();
	}

	@Override
	public String getLabel() {
		return tag;
	}

	@Override
	public boolean isLeaf() {
		return children.isEmpty();
	}

	@Override
	public String toString() {
		return "<" + tag + ">" + StringUtils.join(children, "") + "</" + tag
				+ ">";
	}

	@Override
	public int size() {
		return children.size();
	}
}
