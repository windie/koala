package com.linxiaoyan.wikimirs

// First, how to know - is a unary operator or a binary operator?
// 
trait ExpressionNode {
  def isOperator = this.isInstanceOf[Operator]
  def isOperand = this.isInstanceOf[Operand]
}

case class Operator(left: Option[ExpressionNode], operator: String, right: Option[ExpressionNode]) extends ExpressionNode {

  def isLeftAssociative = {

  }

  // -1
  // 1 + -1
  // ( -1 + 1)

  def isUnaryOperator() = {
    !left.isDefined
  }

  def isRightAssociative = {
    operator == "!"
  }
}

object Operator {
  def build(nodes: List[MathmlNode]) {
	  
  }
}

case class Operand extends ExpressionNode {

}

class Expression {

  def isUnaryOperator(prev: MathmlNode): Boolean = {
    if (prev == null) {
      true
    } else {
      false
    }
  }

  def priority(prev: MathmlNode, operator: MathmlNode) {
    if (prev == null) {

    }
  }
}