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
			String gene = getChildrenTagString(node);
			String nonGene = getChildrenString(node);
			boolean doNotGenerateNonGene = false;
			if (node.getLabel().equals("math")
					|| node.getLabel().equals("mrow")) {
				if (node.size() == 1) {
					if (isNaiveNode(node.iterator().next())) {
					} else {
						doNotGenerateNonGene = true;
					}
				}
			} else {
				gene = getTagString(node, gene);
				nonGene = getTagString(node, nonGene);
			}
			if (!gene.equals(nonGene)) {
				tokens.add(new FormulaTerm(gene, level, true));
			}
			if (!doNotGenerateNonGene) {
				tokens.add(new FormulaTerm(nonGene, level, false));
			}
			for (MathmlNode child : node) {
				toTokens(child, level + 1, tokens);
			}
		}
	}

	private boolean isNaiveNode(MathmlNode node) {
		return !node.isLeaf()
				&& (node.getLabel().equals("mi")
						|| node.getLabel().equals("mo") || node.getLabel()
						.equals("mn"));
	}

	private boolean isOperator(MathmlNode node) {
		return !node.isLeaf() && node.getLabel().equals("mo");
	}

	private String getTagString(MathmlNode node) {
		return "<" + node.getLabel() + "></" + node.getLabel() + ">";
	}

	private String getTagString(MathmlNode node, String content) {
		return "<" + node.getLabel() + ">" + content + "</" + node.getLabel()
				+ ">";
	}

	private String getChildrenTagString(MathmlNode node) {
		StringBuilder builder = new StringBuilder();
		for (MathmlNode child : node) {
			if (child.isText()) {
				builder.append(child.getLabel());
			} else if (isOperator(child)) {
				builder.append(child.toString());
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
