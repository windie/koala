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

class NaiveNode(text: String, tag: String) extends MathmlLeafNode(text, tag)

class MI(text: String) extends NaiveNode(text, "mi") {
}

class MN(text: String) extends NaiveNode(text, "mn") {
}

class MO(text: String, val children: List[MathmlNode] = List[MathmlNode]()) extends MathmlLeafNode(text, "mo") {
  var operantSize = 0
  override val toString = s"<mo o='${text}'>${children.mkString}</mo>"
  override val isLeaf = false
}

class MS(text: String) extends MathmlLeafNode(text, "ms") {
}

class MathmlTokenizer {

  def toTokens(node: MathmlNode, tokens: ListBuffer[FormulaTerm]) {
    toTokens(node, 1, tokens);
  }

  def toTokens(node: MathmlNode, level: Int, tokens: ListBuffer[FormulaTerm]): Unit = node match {
    case node if isNaiveNode(node) => {
      val text = node.asInstanceOf[NaiveNode].text;
      if (text.length != 1) {
        tokens += new FormulaTerm(text, level, false);
      }
      return
    }
    case node if node.isLeaf => {
      if (node.isText && node.label.length == 1) {
        return
      }
      tokens += new FormulaTerm(node.toString, level, false);
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

  private def isNaiveNode(node: MathmlNode) = {
    node.isInstanceOf[NaiveNode]
  }

  private def isOperator(node: MathmlNode) = node.isInstanceOf[MO]

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
