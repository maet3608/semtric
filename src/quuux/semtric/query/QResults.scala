package quuux.semtric.query

import scala.collection.immutable.Map
import quuux.semtric.Item
import quuux.semtric.INone
import quuux.semtric.Item._

/*
 * A query results. Consists of a result item, variables bound to items
 * and possible sub results.
 * Query results are immutable.
 */
class QResult(val item: AnyItem, val variables: Map[String, AnyItem], val subs: Iterator[QResult]) {

  // Returns a new QResult with the given result item.
  def item[T](item: Item[T]) = new QResult(item, variables, subs)

  // Returns a new QResult with a variable and result item bound to the given value.
  def bind[T](varname: String, value: Item[T]) =
    new QResult(value, variables + (varname -> value), subs)

  // Returns a new QResult with the given sub results.
  def subs(newSubs: Iterator[QResult]): QResult =
    new QResult(item, variables, subs ++ newSubs)

  // Returns an option with the value of the given variable or None.
  def variable(varname: String): Option[AnyItem] = variables.get(varname)

  override def hashCode(): Int = item.hashCode() + 7 * variables.hashCode()

  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case r: QResult => this.item.value == r.item.value &&
        this.variables == r.variables
      case _ => false
    }
  }

  // Returns a string representation of the result object.
  override def toString() = toString(1)
  def toString(level: Int): String = {
    val indent = "\n" + "   " * level
    variables.map { case (n, v) => n + "=" + v }.mkString("(" + item + ") ", ", ", "") +
      subs.map(indent + _.toString(level + 1)).mkString
  }
}

object QResult {
  private def noVariables = Map[String, AnyItem]()
  private val noSubs = Iterator.empty
  def apply() = new QResult(INone, noVariables, noSubs)
  def apply[T](item: Item[T]) = new QResult(item, noVariables, noSubs)
  def apply(variables: Map[String, AnyItem]) = new QResult(INone, variables, noSubs)
  def apply(item: AnyItem, variables: Map[String, AnyItem]) = new QResult(item, variables, noSubs)
}