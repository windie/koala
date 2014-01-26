package com.linxiaoyan.wikimirs

import org.apache.commons.lang3.StringUtils
import scala.collection.mutable.ListBuffer

trait MathmlNode extends Ordered[MathmlNode] {

  def parent: MathmlNode

  def children: List[MathmlNode]
  def compare(that: MathmlNode) = {
    this.toString.compare(that.toString)
  }
}

class MathmlTag(val parent: MathmlTag, val tag: String,
  _children: ListBuffer[MathmlNode] = ListBuffer[MathmlNode]()) extends MathmlNode {

  def children: List[MathmlNode] = _children.toList

  def addChild(child: MathmlNode) {
    _children += child
  }

  val tagString = s"<${tag}></${tag}>"

  def tagString(content: String): String = s"<${tag}>${content}</${tag}>"

  override def toString() =
    s"<${tag}>${children.mkString}</${tag}>";
}

class Text(val parent: MathmlTag, val text: String) extends MathmlNode {
  override val toString = text
  val children = Nil
}

//class MSpace extends MathmlNode {
//
//  override val label: String = "mspace"
//  override val toString = "<mspace/>"
//  override val size = 0
//}

trait TagText extends MathmlNode {
  // Do not use parent
  def parent: MathmlNode = null

  def tag: String
  def text: String
  val tagString = s"<${tag}></${tag}>"
  override val toString = s"<${tag}>${text}</${tag}>"
}

abstract class NaiveNode(val text: String, val tag: String) extends TagText {
  val children = Nil
}

class MText(text: String) extends NaiveNode(text, "mtext") {
}

class MI(text: String) extends NaiveNode(text, "mi") {
}

class MN(text: String) extends NaiveNode(text, "mn") {
}

class MO(val text: String, val children: List[MathmlNode] = List[MathmlNode]()) extends TagText {
  val tag = "mo"
  var operantSize = 0
  override val toString = s"<mo o='${text}'>${children.mkString}</mo>"
  override val tagString = s"<mo o='${text}'></mo>"
  def tagString(content: String): String = s"<mo o='${text}'>${content}</mo>"
}

class MS(text: String) extends NaiveNode(text, "ms") {
}

class MathmlTokenizer(val enableGen: Boolean = true) {

  def toTokens(node: MathmlNode, tokens: ListBuffer[FormulaTerm]) {
    toTokens(node, 1, tokens);
  }

  def toTokens(node: MathmlNode, level: Int, tokens: ListBuffer[FormulaTerm]): Unit = node match {
    case node: NaiveNode => {
      val text = node.text
      if (text.length > 1) {
        tokens += new FormulaTerm(node.toString, level, false)
      }
    }
    case node: Text => {
      if (node.text.length <= 1) {
        return
      }
      tokens += new FormulaTerm(node.toString, level, false)
    }
    case node: MO => {
      val generalized = node.tagString(getChildrenTagString(node))
      val nonGeneralized = node.tagString(getChildrenString(node))
      if (!generalized.equals(nonGeneralized)) {
        if (enableGen) {
          tokens += new FormulaTerm(generalized, level, true)
        }
      }
      tokens += new FormulaTerm(nonGeneralized, level, false)
      node.children.foreach {
        toTokens(_, level + 1, tokens);
      }
    }
    case node: MathmlTag => {
      var generalized = getChildrenTagString(node);
      var nonGeneralized = getChildrenString(node);
      var skipThisNode = false;
      if (node.tag.equals("math")
        || node.tag.equals("mrow")) {
        if (node.children.size == 1) {
          // <math><mi>a</mi></math> 
          // nonGeneralized => <mi>a</mi>
          // already generated
          skipThisNode = true
        }
      } else {
        generalized = node.tagString(generalized)
        nonGeneralized = node.tagString(nonGeneralized)
      }
      if (!skipThisNode) {
        if (!generalized.equals(nonGeneralized)) {
          if (enableGen) {
            tokens += new FormulaTerm(generalized, level, true)
          }
        }
        tokens += new FormulaTerm(nonGeneralized, level, false);
      }
      node.children.foreach {
        toTokens(_, level + 1, tokens);
      }
    }
  }

  private def getChildrenTagString(node: MathmlNode): String = {
    node.children.map {
      case o: Text => o.text
      case o: TagText => o.tagString
      case o: MathmlTag => o.tagString
    }.mkString
  }

  private def getChildrenString(node: MathmlNode) = {
    node.children.map(_.toString).mkString
  }
}
