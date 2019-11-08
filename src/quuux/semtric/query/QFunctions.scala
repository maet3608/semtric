package quuux.semtric.query

import quuux.semtric._
import quuux.semtric.Item._
import scala.util.Sorting

// useful?
//abstract class QFunction(argument: QNode) extends QNode
//test number and type of arguments

class QFKind(argument: QNode, kind: QValue[String]) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    argument.matches(network, thiz, result).filter(_.item.kind == kind.value)
  }
  override def toString() = "?kind(" + argument + ", " + kind.value + ")"
}

class QFRegEx(argument: QNode, regex: QValue[String]) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    argument.matches(network, thiz, result).filter(_.item.value.toString.matches(regex.value))
  }
  override def toString() = "?regex(" + argument + ", " + regex.value + ")"
}

class QFCount(argument: QNode) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    val results = argument.matches(network, thiz, result)
    val name = argument.toString() // argument of count call as variable name.
    val count = IInt(results.size) // count as integer item.
    List(QResult(count, Map(name -> count))).iterator
  }
  override def toString() = "?count(" + argument + ")"
}

class QFLen(argument: QNode) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    val results = argument.matches(network, thiz, result)
    val name = "len"
    results.map(r => r.bind(name, IInt(r.item.value.toString.length)))
  }
  override def toString() = "?len(" + argument + ")"
}

class QFTake(argument: QNode, number: QValue[Int]) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    argument.matches(network, thiz, result).take(number.value)
  }
  override def toString() = "?take(" + argument + ", " + number + ")"
}

class QFFor(arg_cond: QNode, arg_body: QNode) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    arg_cond.matches(network, thiz, result).map(r => 
      r.subs(arg_body.matches(network, r.item, QResult())))
  }
  //override def toString() = "?take(" + argument + ", " + number + ")"
}

class QFUnique(argument: QNode) extends QNode {
  private var results = Set[QResult]()
  private def isNew(result: QResult) =
    if (results.contains(result)) false else { results += result; true }

  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    argument.matches(network, thiz, result).filter(isNew)
  }
  override def toString() = "?unique(" + argument + ")"
}

class QFSort(argument: QNode, varname: QValue[String]) extends QNode {
  object ResultOrdering extends Ordering[QResult] {
    def value(r:QResult) = r.variable(varname.value).get
    def compare(r1:QResult, r2:QResult) = value(r1) compare value(r2) 
  }
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    val results = argument.matches(network, thiz, result).toArray
    Sorting.quickSort(results)(ResultOrdering)
    results.iterator
  }
  override def toString() = "?sort(" + argument + ", " + varname + ")"
}

object QFunction {
  def apply(name: String, args: List[QNode]) = name match {
    case "kind"    => new QFKind(args(0), args(1).asInstanceOf[QValue[String]])
    case "regex"   => new QFRegEx(args(0), args(1).asInstanceOf[QValue[String]])
    case "count"   => new QFCount(args(0))
    case "len"     => new QFLen(args(0))
    case "unique"  => new QFUnique(args(0))
    case "take"    => new QFTake(args(0), args(1).asInstanceOf[QValue[Int]])
    case "for"     => new QFFor(args(0), args(1))
    case "sort"    => new QFSort(args(0), args(1).asInstanceOf[QValue[String]])
    case _         => throw new IllegalArgumentException("Unknown function: " + name)
  }
}