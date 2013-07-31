package com.linxiaoyan.wikimirs;

import java.util.ArrayList;
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

	private void rebuild(MathmlNode node) {
		if (node.isLeaf()) {
			return;
		}
		if (isNaiveNode(node)) {
			return;
		}
		List<MathmlNode> operators = new ArrayList<MathmlNode>();
		List<MathmlNode> operands = new ArrayList<MathmlNode>();
		for (MathmlNode child : node) {
			if (isOperator(node)) {
				while (!operators.isEmpty()) {
					MathmlNode top = operators.get(operators.size() - 1);
					if (isLess(top, child)) {
						operators.add(child);
						break;
					} else {
						if (isBinaryOperator(top)) {
							if (operands.isEmpty()) {
								MathmlTag operator = newOperatorNode(top);
								operands.add(operator);
							} else if (operands.size() == 1) {
								MathmlNode topOperand = operands.get(operands
										.size() - 1);
								MathmlTag operator = newOperatorNode(top);
								operator.addChild(topOperand);
								topOperand.setParent(operator);
								operands.add(operator);
							} else if (operands.size() > 1) {
								MathmlNode topOperand1 = operands.get(operands
										.size() - 1);
								MathmlNode topOperand2 = operands.get(operands
										.size() - 2);
								MathmlTag operator = newOperatorNode(top);
								operator.addChild(topOperand1);
								operator.addChild(topOperand2);
								topOperand1.setParent(operator);
								topOperand2.setParent(operator);
								operands.add(operator);
							}
						} else {
							// unary
							if (operands.isEmpty()) {
								MathmlTag operator = newOperatorNode(top);
								operands.add(operator);
							} else {
								MathmlNode topOperand = operands.get(operands
										.size() - 1);
								MathmlTag operator = newOperatorNode(child);
								operator.addChild(topOperand);
								topOperand.setParent(operator);
								operands.add(operator);
							}
						}
					}
				}
			} else {
				operands.add(child);
			}
		}
	}

	private MathmlTag newOperatorNode(MathmlNode node) {
		return new MathmlTag(node.getParent(), getNaiveNodeLabel(node));
	}

	private boolean isBinaryOperator(MathmlNode node) {
		return true;
	}

	private boolean isLess(MathmlNode node1, MathmlNode node2) {
		return true;
	}

	private int getOperatorPriority(MathmlNode operator) {
		String sign = getNaiveNodeLabel(operator);
		if (sign.equals("-")) {
			// unary - ?? how
			return 2;
		}
		if (sign.equals("^")) {
			return 3;
		}
		if (sign.equals("*") || sign.equals("/") || sign.equals("%")) {
			return 4;
		}
		if (sign.equals("+") || sign.equals("-")) {
			return 5;
		}
		if (sign.equals("<") || sign.equals("<=") || sign.equals(">")
				|| sign.equals(">=")) {
			return 6;
		}
		if (sign.equals("==") || sign.equals("!=")) {
			return 7;
		}
		if (sign.equals("&&")) {
			return 8;
		}
		if (sign.equals("||")) {
			return 9;
		}
		if (sign.equals("=")) {
			return 10;
		}
		return 1;
	}

	private String getNaiveNodeLabel(MathmlNode node) {
		return node.iterator().next().getLabel();
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
