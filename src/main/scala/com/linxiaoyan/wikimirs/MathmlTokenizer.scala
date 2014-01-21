package com.linxiaoyan.wikimirs

import org.apache.commons.lang3.StringUtils
import scala.collection.mutable.ListBuffer

trait MathmlNode extends Iterable[MathmlNode] with Ordered[MathmlNode] {

  def isText: Boolean

  def isLeaf: Boolean

  def label: String

  def parent: MathmlNode

  def size: Int

  def compare(that: MathmlNode) = {
    // TODO improve the performance
    this.toString.compare(that.toString)
  }
}

class MathmlTag(val parent: MathmlTag, val tag: String,
  val children: ListBuffer[MathmlNode] = ListBuffer[MathmlNode]()) extends MathmlNode {

  override val isText = false

  def addChild(child: MathmlNode) {
    children += child
  }

  override val label: String = tag

  override def isLeaf: Boolean = children.isEmpty

  override def toString() =
    "<" + tag + ">" + children.mkString + "</" + tag + ">";

  override def size = children.size

  override def iterator = children.iterator
}

class MathmlText(val parent: MathmlTag, val text: String) extends MathmlNode {

  override val isText = true

  override def iterator = List[MathmlText]().iterator

  override val label: String = text

  override val isLeaf = true

  override val toString = text

  override val size = 0
}

abstract class MathmlLeafNode(val text: String, val tag: String) extends MathmlNode {
  override val isText = false

  override def iterator = List[MathmlText]().iterator

  override val isLeaf = true

  override val toString = s"<${tag}>${text}</${tag}>"

  override val size = 0

  override val label: String = tag

  override val parent: MathmlNode = null
}

class MSpace(val parent: MathmlTag) extends MathmlNode {
  override val isText = false

  override def iterator = List[MathmlText]().iterator

  override val label: String = "mspace"

  override val isLeaf = true

  override val toString = "<mspace/>"

  override val size = 0
}

class MText(text: String) extends MathmlLeafNode(text, "mtext") {
}

class MI(text: String) extends MathmlLeafNode(text, "mi") {
}

class MN(text: String) extends MathmlLeafNode(text, "mn") {
}

class MO(text: String, val children: List[MathmlNode] = List[MathmlNode]()) extends MathmlLeafNode(text, "mo") {
  var operantSize = 0
  override val toString = s"<mo o='${text}'>${children.mkString}</mo>"
}

class MS(text: String) extends MathmlLeafNode(text, "ms") {
}

class MathmlTokenizer {

  def toTokens(node: MathmlNode, tokens: ListBuffer[FormulaTerm]) {
    toTokens(node, 1, tokens);
  }

  def toTokens(node: MathmlNode, level: Int, tokens: ListBuffer[FormulaTerm]): Unit = node match {
    case node if node.isLeaf => {
      if (node.isText && node.label.length == 1) {
        return
      }
      tokens += new FormulaTerm(node.toString, level, false);
    }
    case node if isNaiveNode(node) => {
      val text = node.iterator.next.label;
      if (text.length != 1) {
        tokens += new FormulaTerm(text, level, false);
      }
      return ;
    }
    case node => {
      var gene = getChildrenTagString(node);
      var nonGene = getChildrenString(node);
      var doNotGenerateNonGene = false;
      if (node.label.equals("math")
        || node.label.equals("mrow")) {
        if (node.size == 1) {
          if (isNaiveNode(node.iterator.next)) {
          } else {
            doNotGenerateNonGene = true;
          }
        }
      } else {
        gene = getTagString(node, gene);
        nonGene = getTagString(node, nonGene);
      }
      if (!gene.equals(nonGene)) {
        tokens += new FormulaTerm(gene, level, true);
      }
      if (!doNotGenerateNonGene) {
        tokens += new FormulaTerm(nonGene, level, false);
      }
      node.foreach {
        toTokens(_, level + 1, tokens);
      }
    }
  }
  //
  //	private void rebuild(MathmlNode node) {
  //		if (node.isLeaf()) {
  //			return;
  //		}
  //		if (isNaiveNode(node)) {
  //			return;
  //		}
  //		List<MathmlNode> operators = new ArrayList<MathmlNode>();
  //		List<MathmlNode> operands = new ArrayList<MathmlNode>();
  //		for (MathmlNode child : node) {
  //			if (isOperator(node)) {
  //				while (!operators.isEmpty()) {
  //					MathmlNode top = operators.get(operators.size() - 1);
  //					if (isLess(top, child)) {
  //						operators.add(child);
  //						break;
  //					} else {
  //						if (isBinaryOperator(top)) {
  //							if (operands.isEmpty()) {
  //								MathmlTag operator = newOperatorNode(top);
  //								operands.add(operator);
  //							} else if (operands.size() == 1) {
  //								MathmlNode topOperand = operands.get(operands
  //										.size() - 1);
  //								MathmlTag operator = newOperatorNode(top);
  //								operator.addChild(topOperand);
  //								topOperand.setParent(operator);
  //								operands.add(operator);
  //							} else if (operands.size() > 1) {
  //								MathmlNode topOperand1 = operands.get(operands
  //										.size() - 1);
  //								MathmlNode topOperand2 = operands.get(operands
  //										.size() - 2);
  //								MathmlTag operator = newOperatorNode(top);
  //								operator.addChild(topOperand1);
  //								operator.addChild(topOperand2);
  //								topOperand1.setParent(operator);
  //								topOperand2.setParent(operator);
  //								operands.add(operator);
  //							}
  //						} else {
  //							// unary
  //							if (operands.isEmpty()) {
  //								MathmlTag operator = newOperatorNode(top);
  //								operands.add(operator);
  //							} else {
  //								MathmlNode topOperand = operands.get(operands
  //										.size() - 1);
  //								MathmlTag operator = newOperatorNode(child);
  //								operator.addChild(topOperand);
  //								topOperand.setParent(operator);
  //								operands.add(operator);
  //							}
  //						}
  //					}
  //				}
  //			} else {
  //				operands.add(child);
  //			}
  //		}
  //	}

  //	private MathmlTag newOperatorNode(MathmlNode node) {
  //		return new MathmlTag(node.getParent(), getNaiveNodeLabel(node));
  //	}
  //
  //	private boolean isBinaryOperator(MathmlNode node) {
  //		return true;
  //	}
  //
  //	private boolean isLess(MathmlNode node1, MathmlNode node2) {
  //		return true;
  //	}

  //	private int getOperatorPriority(MathmlNode operator) {
  //		String sign = getNaiveNodeLabel(operator);
  //		if (sign.equals("-")) {
  //			// unary - ?? how
  //			return 2;
  //		}
  //		if (sign.equals("^")) {
  //			return 3;
  //		}
  //		if (sign.equals("*") || sign.equals("/") || sign.equals("%")) {
  //			return 4;
  //		}
  //		if (sign.equals("+") || sign.equals("-")) {
  //			return 5;
  //		}
  //		if (sign.equals("<") || sign.equals("<=") || sign.equals(">")
  //				|| sign.equals(">=")) {
  //			return 6;
  //		}
  //		if (sign.equals("==") || sign.equals("!=")) {
  //			return 7;
  //		}
  //		if (sign.equals("&&")) {
  //			return 8;
  //		}
  //		if (sign.equals("||")) {
  //			return 9;
  //		}
  //		if (sign.equals("=")) {
  //			return 10;
  //		}
  //		return 1;
  //	}

  private def getNaiveNodeLabel(node: MathmlNode) = node.iterator.next.label

  private def isNaiveNode(node: MathmlNode) = {
    !node.isLeaf &&
      (node.label.equals("mi") || node.label.equals("mo") || node.label.equals("mn"));
  }

  private def isOperator(node: MathmlNode) = !node.isLeaf && node.label.equals("mo")

  private def getTagString(node: MathmlNode) = "<" + node.label + "></" + node.label + ">"

  private def getTagString(node: MathmlNode, content: String) = "<" + node.label + ">" + content + "</" + node.label + ">"

  private def getChildrenTagString(node: MathmlNode): String = {
    node.map {
      case o if o.isText => o.label
      case o if isOperator(o) => o.toString
      case o => getTagString(o)
    }.mkString
  }

  private def getChildrenString(node: MathmlNode) = {
    node.map(_.toString).mkString
  }
}
