package quuux.semtric.query

import quuux.semtric.Network
import quuux.semtric.Relation
import quuux.semtric.Item._

abstract class QOperator(left: QNode, right: QNode) extends QNode {

}

class QAnd(left: QNode, right: QNode) extends QOperator(left, right) {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    for (lr <- left.matches(network, thiz, result); rr <- right.matches(network, thiz, lr)) yield rr
  }
  override def toString() = "[" + left + " & " + right + "]"
}

object QAnd {
  def apply(left: QNode, right: QNode) = new QAnd(left, right)
}

// Exclusive OR.
class QOr(left: QNode, right: QNode) extends QOperator(left, right) {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    left.matches(network, thiz, result) ++ right.matches(network, thiz, result)
  }
  override def toString() = left + " | " + right
}

object QOr {
  def apply(left: QNode, right: QNode) = new QOr(left, right)
}

// Inclusive OR = Alternation.
class QAlt(left: QNode, right: QNode) extends QOperator(left, right) {  
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    (for (lr <- left.matches(network, thiz, result); 
         rightResults = right.matches(network, thiz, lr)) 
     yield if(rightResults.isEmpty) List(lr).iterator else rightResults).flatten
  }
  override def toString() = left + " ; " + right
}

object QAlt {
  def apply(left: QNode, right: QNode) = new QAlt(left, right)
}

// Set difference = Complement.
// Two results are equal if their variable bindings are equal.
class QComplement(left: QNode, right: QNode) extends QOperator(left, right) {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    //val rightResults = right.matches(network, thiz, result).toSet
    //left.matches(network, thiz, result).filterNot(rightResults.contains)
    val rightResultsVars = right.matches(network, thiz, result).map(_.variables).toSet
    left.matches(network, thiz, result).filterNot(lr => rightResultsVars.contains(lr.variables))
  }
  override def toString() = left + " - " + right
}

object QComplement {
  def apply(left: QNode, right: QNode) = new QComplement(left, right)
}

class QRelation(left: QNode, right: QNode) extends QOperator(left, right) {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    val leftResults = left.matches(network, thiz, result)
    for (lr <- leftResults; rr <- right.matches(network, lr.item, lr)) yield rr
  }
  override def toString() = left + " " + right
}

object QRelation {
  def apply(left: QNode, right: QNode) = new QRelation(left, right)
}
