package quuux.semtric.test

import org.scalatest._
import quuux.semtric._

class RelationTest extends FlatSpec with Matchers {

  "Relation" should "be equal if their items are equal" in {
    val (item1, item2, item3) = (IInt(1), IInt(2), IInt(1))
    assert(Relation(item1, item2) == Relation(item1, item2))
    assert(Relation(item1, item2) != Relation(item1, item3))
  }

  it should "be directional" in {
    val (item1, item2) = (IInt(1), IInt(2))
    assert(Relation(item1, item2) != Relation(item2, item1))
  }

  it should "be valid to have relations between identical items" in {
    val item = IInt(1)
    Relation(item, item)
  }
  
  it should "support implict item conversion" in {
    val relation1 = Relation("String", 12)
    assert(relation1.thiz.isInstanceOf[IString])
    assert(relation1.that.isInstanceOf[IInt])
    
    val relation2 = Relation(3.14, true)
    assert(relation2.thiz.isInstanceOf[IReal])
    assert(relation2.that.isInstanceOf[IBool])
  }
}  