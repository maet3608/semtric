package quuux.semtric.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._

class NetworkTest extends FlatSpec with Matchers {
  
  def fixture = new {
    val (item1, item2, item3) = (IInt(1), IInt(2), IInt(3))
  }

  "Network" should "have no item in relations that is not in items" in {
    val f = fixture
    val relations: Vector[AnyRelation] = Vector(Relation(f.item1, f.item2))
    Network(Vector(f.item1, f.item2), relations).isValid should be (true)
    Network(Vector(f.item1, f.item3), relations).isValid should be (false)
    Network(Vector(f.item1, f.item2, f.item3), relations).isValid should be (true)
  }

  it should "have valid constructor given relations only" in {
    val f = fixture
    val relations: Vector[AnyRelation] = Vector(Relation(f.item1, f.item2), Relation(f.item1, f.item3))
    Network(relations).isValid should be (true)
  }
  
  it should "be equal to other network if items and releations are equal" in {
    val f = fixture
    val network1 = Network(Vector(Relation(f.item1, f.item2), Relation(f.item1, f.item3)))
    val network2 = Network(Vector(Relation(f.item1, f.item2), Relation(f.item1, f.item3)))
    val network3 = Network(Vector(Relation(f.item1, f.item3)))
    assert(network1 == network2)
    assert(network1 != network3)
  }  
  
  it should "return child items of a given value" in {
    val (item1, item2, item3) = (IInt(1), IInt(2), IInt(2))
    val network = Network(Vector(Relation(item1, item2), Relation(item1, item3)))
    network.thats(item1, 2).toList should be (List(item2,item3))
    network.thats(item2, 2).toList should be (List())
  }    

  it should "allow multiple identical relations" in {
    val (item1, item2) = (IInt(1), IInt(2))
    val relation = Relation(item1, item2)
    val network = Network(Vector(relation, relation, relation))
    network.thats(item1).toVector should be (List(item2,item2,item2))
  }   
  
  it should "preserve order of relations" in {
    val (item1, item2, item3) = (IInt(1), IInt(2), IInt(2))
    val relations = Vector(Relation(item1, item3), Relation(item1, item2), Relation(item1, item3))
    val network = Network(relations)
    network.relations.toVector should be (relations)
    network.thats(item1).toVector should be (List(item3,item2,item3))
  }      
  
}