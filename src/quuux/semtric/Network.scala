package quuux.semtric

import scala.collection.immutable.Map
import Item._
import Relation._
import scala.collection.immutable.LinearSeq
import scala.collection.immutable.ListSet

/**
 * A semantic Network is an immutable collection of relations between items.
 * There can be items that are not in relations.
 */
abstract class Network {
  /** Iterator over the network items of. */
  def items: Iterator[AnyItem]
  /** Iterator over the network relations. */
  def relations: Iterator[AnyRelation]
  /** Returns an iterator over all items with the given value. Should have O(1) time complexity. */
  def items(value: Any): Iterator[AnyItem]
  /** Returns an iterator over all children of the given item. Should have O(1) time complexity. */
  def thats(thiz: AnyItem): Iterator[AnyItem]

  /** Returns an iterator over all children of the given item that have the given value. */
  def thats[T](thiz: AnyItem, value: T): Iterator[AnyItem] =
    thats(thiz).filter(_.value == value)

  /** Returns set of all items that have no parents. */
  def roots: Set[AnyItem] = relations.map(_.thiz).toSet -- relations.map(_.that)

  /** Returns true if all items in relations are in the set of items. */
  def isValid = {
    val itemsInRelations = Set[AnyItem]() ++ relations.map(_.thiz) ++ relations.map(_.that)
    // There can be more items than in relations but not less.
    itemsInRelations.subsetOf(items.toSet)
  }

  /** Traverse the network in a tree-like fashion. Network can contain cycles.
   *  Calls function f(item, level, call) for each item and passes on the
   *  level of the item in the tree, and the number of times f has been called.
   *  maxLevel allows to limit the depth to traverse and maxCalls limits the number of calls.
   */
//  def traverse(f: (AnyItem, Int, Int) => Unit, maxLevel: Int = 0, maxCalls: Int = 0) {
//    var calls = 0
//    def traverse(item: AnyItem, level: Int = 0, touched: Set[AnyItem] = Set()) {
//      if (maxLevel > 0 && level >= maxLevel) return
//      if (maxCalls > 0 && calls >= maxCalls) return
//      if (touched.contains(item)) return else { calls += 1; f(item, level, calls) }
//      thats(item).foreach(traverse(_, level + 1, touched + item))
//    }
//    if (items.isEmpty) return
//    (roots match { case r => if (r.isEmpty) Set(items.next()) else r }) foreach(traverse(_))
//  }
  
  def traverse[T](f: (AnyItem, T, Int, Int) => T, node:T, maxLevel: Int = 0, maxCalls: Int = 0) {
    var calls = 0
    def traverse(item: AnyItem, node:T, level: Int = 0, touched: Set[AnyItem] = Set()) {
      var child = node
      if (maxLevel > 0 && level >= maxLevel) return
      if (maxCalls > 0 && calls >= maxCalls) return
      if (touched.contains(item)) return else { calls += 1; child = f(item, node, level, calls) }
      thats(item).foreach(traverse(_, child, level + 1, touched + item))
    }
    if (items.isEmpty) return
    (roots match { case r => if (r.isEmpty) Set(items.next()) else r }) foreach(traverse(_, node))
  }
  
  
  def print(showUUID: Int = 0, showKind: Boolean = false, maxLevel: Int = 0, maxLines: Int = 0) {
    def print(item: AnyItem, node:Any, level: Int, lines :Int) = {
      val uuid = if (showUUID > 0) "@" + item.uuid.take(showUUID) else ""
      val kind = if (showKind) "|" + item.kind else ""
      println("  " * level + item.value + uuid + kind)
    }
    println("Network" + "-" * 73)
    traverse(print, None, maxLevel, maxLines)
    println("-" * 80)
  }

}

/**
 * An in-memory network. When constructing ensure that all items used in relations are in the
 * set of items. Items are a set but relations should be some ordered iterable to preserve
 * the order of parent to child relations.
 */
class NetworkInMemory(private val _items: Iterable[AnyItem],
                      private val _relations: Iterable[AnyRelation])
  extends Network {

  def items: Iterator[AnyItem] = _items.iterator
  def relations: Iterator[AnyRelation] = _relations.iterator

  // All items with the given value.
  def items(value: Any): Iterator[AnyItem] = _items.iterator.filter(_.value == value)

  // All children (=thats) of the thiz item.
  private val thiz2that = _relations.groupBy(_.thiz).mapValues(_.map(_.that).iterator)
  def thats(thiz: AnyItem): Iterator[AnyItem] = thiz2that.getOrElse(thiz, Iterator.empty)

  /**
   * Networks are equal if their item and relations are equal and in the same order!
   * For large, similar networks this is an expensive comparison!
   */
  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case n: Network => this.items.toList == n.items.toList &&
        this.relations.toList == n.relations.toList
      case _ => false
    }
  }

  override def hashCode(): Int = _items.hashCode() + 7 * _relations.hashCode()

  override def toString() = {
    _items.map(_.toString).mkString("\n")
    _relations.map(_.toString).mkString("\n")
  }
}

/** Network factory. */
object Network {
  /** Construct empty network. */
  def apply(): Network = Network(Vector.empty, Vector.empty)

  /**
   * Construct Network from items and relations. There can be items that are not in relations.
   *  Items preferably should be unique (e.g. in a Set) but is not enforced.
   *  Relations are ordered to preserve the ordering of the children of a parent, for instance.
   */
  def apply(items: Vector[AnyItem], relations: Vector[AnyRelation]): Network =
    new NetworkInMemory(items, relations)

  /** Constructs Network from relations only. Items are extracted automatically. */
  def apply(relations: Vector[AnyRelation]): Network = {
    val items = Set[AnyItem]() ++ relations.map(_.thiz) ++ relations.map(_.that)
    Network(items.toVector, relations)
  }
}