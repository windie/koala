package com.linxiaoyan.wikimirs;

import java.util.List;

public class MathmlTokenizer {

	public void toTokens(MathmlNode node, List<FormulaTerm> tokens) {
		toTokens(node, 1, tokens);
	}

	private void toTokens(MathmlNode node, int level, List<FormulaTerm> tokens) {
		if (node.isLeaf()) {
			if (node.isText() && node.getLabel().length() == 1) {
				return;
			}
			tokens.add(new FormulaTerm(node.toString(), level, false));
		} else if (isNaiveNode(node)) {
			String text = node.iterator().next().getLabel();
			if (text.length() != 1) {
				tokens.add(new FormulaTerm(text, level, false));
			}
			return;
		} else {
			tokens.add(new FormulaTerm(getChildrenTagString(node), level, true));
			tokens.add(new FormulaTerm(getChildrenString(node), level, false));
			for (MathmlNode child : node) {
				toTokens(child, level + 1, tokens);
			}
		}
	}

	private boolean isNaiveNode(MathmlNode node) {
		return !node.isLeaf()
				&& (node.getLabel().equals("mi")
						|| node.getLabel().equals("mc")
						|| node.getLabel().equals("mo") || node.getLabel()
						.equals("mn"));
	}

	private String getTagString(MathmlNode node) {
		return "<" + node.getLabel() + "></" + node.getLabel() + ">";
	}

	private String getChildrenTagString(MathmlNode node) {
		StringBuilder builder = new StringBuilder();
		for (MathmlNode child : node) {
			if (child.isText()) {
				builder.append(child.getLabel());
			} else {
				builder.append(getTagString(child));
			}
		}
		return builder.toString();
	}

	private String getChildrenString(MathmlNode node) {
		StringBuilder builder = new StringBuilder();
		for (MathmlNode child : node) {
			builder.append(child.toString());
		}
		return builder.toString();
	}
}
