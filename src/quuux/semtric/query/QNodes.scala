package quuux.semtric.query

import quuux.semtric.Item._
import quuux.semtric._
import java.util.regex.Pattern
import java.util.regex.Matcher

/**
 * Base class for all nodes/elements of a query. Apart from results every other element
 * of a query such as values, variables, functions or operators are QNodes.
 * A QNode has a match method that returns an iterator over the result it matches.
 * For instance, a simple query that has a single QValue will match only the item
 * equal to the QValue's value.
 */
abstract class QNode {
  /**
   * Returns the results this node matches. For instance a QValue will match all items
   * of a network that are identical to the QValue's value. The thiz item specifies a
   * further requirement for a successful match. Only QNodes that have a parent
   * (= there exists a Relation(thiz, QNode) that with the thiz item will match.
   * If thiz==INone, thiz is ignored. Finally, the match method takes a previous/current
   * result and takes it into account when performing the match, e.g. variable bindings.
   */
  def matches(network: Network, thiz: AnyItem, result: QResult): Iterator[QResult]

  /**
   * Returns all thats for the given thiz item. If thiz == INone returns all items of the
   * network.
   */
  def thatsOrAll(network: Network, thiz: AnyItem): Iterator[AnyItem] =
    if (thiz == INone) network.items else network.thats(thiz)
}

/**
 * Describes a specific value. Matches all items with this value, provided items parent == thiz.
 * If thiz == INone, the parent doesn't matter.
 */
class QValue[T](val value: T) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    var items = network.items(value)
    if (thiz != INone) {
      val thats = network.thats(thiz).toSet
      items = items.filter(thats.contains)
    }
    items.map(result.item(_))
  }

  override def toString() = "'" + value.toString() + "'"
}

object QValue {
  def apply[T](value: T) = new QValue(value)
}

/**
 * Matches all items which value matches the regular expression,  provided items parent == thiz.
 * If thiz == INone, the parent doesn't matter.
 * NOTE: This is SLOW for thiz == INone, since ALL items of the network need to be compared
 * against the regular expression.
 */
class QRegEx(val regex: String) extends QNode {
  private val pattern = Pattern.compile(regex) // Java: because matching is simpler in this case.
  def matches(network: Network, thiz: AnyItem, result: QResult) = {
    val items = thatsOrAll(network, thiz)
    items.filter(i => pattern.matcher(i.value.toString).matches).map(result.item(_))
  }

  override def toString() = "/" + regex + "/"
}

object QRegEx {
  def apply(regex: String) = new QRegEx(regex)
}

/**
 * Describes a variable. It matches against all items (that have parent == thiz, provided
 * thiz != INone) if not already bound to a specific item.
 * The binding is stored and read from the results. If the variable is not bound, it will be
 * bound to the given variable name and the binding is added to the output result.
 * If the variable is bound it matches only against the bound item (taking thiz into account).
 * If the variable name is empty it matches against all items (taking thiz into account).
 * Empty variable names are not bound to specific items.
 */
class QVariable(val varname: String) extends QNode {
  def matches(network: Network, thiz: AnyItem, result: QResult): Iterator[QResult] = {
    val items = thatsOrAll(network, thiz)
    if (varname.isEmpty) return items.map(result.item(_))
    result.variable(varname) match {
      case Some(item) => items.filter(_ == item).map(result.item(_))
      case None       => items.map(result.bind(varname, _))
    }
  }

  override def toString() = "<" + varname.toString() + ">"
}

object QVariable {
  def apply(varname: String) = new QVariable(varname)
}
