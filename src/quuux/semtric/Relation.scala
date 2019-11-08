package quuux.semtric

import quuux.semtric.Relation._

/**
 * A relation between two network items. 
 * Note that relations are directed and are links/edges between nodes/items in the
 * network/graph.
 */
class Relation[S, O](val thiz: Item[S], val that: Item[O]) {

  /** Relations are equal if their thiz and that items are equal. Relations are directed. */
  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case r: AnyRelation => this.thiz == r.thiz && this.that == r.that
      case _ => false
    }
  }

  override def hashCode(): Int = thiz.hashCode() + 7 * that.hashCode()
  
  override def toString() = "(" + thiz + "," + that + ")"
}

/** Relation factory. */
object Relation {  
  type AnyRelation = Relation[_, _]

  def apply[S, O](thiz: Item[S], that: Item[O]) = new Relation(thiz, that)
}