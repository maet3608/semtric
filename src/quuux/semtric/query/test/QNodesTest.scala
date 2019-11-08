package quuux.semtric.query.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import quuux.semtric.query._
import java.util.regex.Pattern

class QNodesTest extends FlatSpec with Matchers {

  def fixture = new {
    val iItem0 = IString("Oganizer")
    val iItem1 = IString("Contacts")
    val iItem2 = IString("Contact")
    val iItem3 = IString("Contact")
    val iItem5 = IString("Private")

    val relations: Vector[AnyRelation] =
      Vector(Relation(iItem0, iItem1), Relation(iItem1, iItem2), Relation(iItem1, iItem3),
        Relation(iItem1, iItem5))
    val network = Network(relations)
  }

  class QNodeTest extends QNode {
    def matches(network: Network, thiz: AnyItem, result: QResult) = Iterator.empty
  }

  "QNode.thatsOrAll" should "return all network items for NONE" in {
    val f = fixture
    val node = new QNodeTest
    val items = f.network.items.toSet
    node.thatsOrAll(f.network, INone).toSet should be(items)
  }

  it should "return all children for an item" in {
    val f = fixture
    val node = new QNodeTest
    for (item <- f.network.items) {
      val thats = f.network.relations.filter(r => r.thiz == item).map(_.that).toSet
      node.thatsOrAll(f.network, item).toSet should be(thats)
    }
  }

  "QValue" should "should match all network items with the same value for thiz == INone" in {
    val f = fixture
    for (item <- f.network.items) {
      val node = new QValue(item.value)
      val items = f.network.items.filter(_.value == item.value).toSet
      val results = node.matches(f.network, INone, QResult())
      results.map(_.item).toSet should be(items)
    }
  }

  it should "should match all items with the same value and the given thiz parent" in {
    val f = fixture
    for (relation <- f.network.relations) {
      val thiz = relation.thiz
      val value = relation.that.value
      val node = new QValue(value)
      val items = f.network.relations.filter(r => r.thiz == thiz && r.that.value == value)
        .map(_.that).toSet
      val results = node.matches(f.network, thiz, QResult())
      results.map(_.item).toSet should be(items)
    }
  }

  "QRegEx" should "should match all network items, which value matches the regex for thiz == INone" in {
    val f = fixture
    val regex = "Con.+"
    val pattern = Pattern.compile(regex)
    for (item <- f.network.items) {
      val node = new QRegEx(regex)
      val items = f.network.items.filter(i => pattern.matcher(i.value.toString).matches).toSet
      val results = node.matches(f.network, INone, QResult())
      results.map(_.item).toSet should be(items)
    }
  }

  it should "should match all network items, which value matches the regex and have the given thiz parent" in {
    val f = fixture
    val regex = "Con.+"
    val pattern = Pattern.compile(regex)
    for (relation <- f.network.relations) {
      val thiz = relation.thiz
      val node = new QRegEx(regex)
      val items = f.network.relations
        .filter(r => r.thiz == thiz && pattern.matcher(r.that.value.toString).matches)
        .map(_.that).toSet
      val results = node.matches(f.network, thiz, QResult())
      results.map(_.item).toSet should be(items)
    }
  }
  
  "QVariable" should "should match all network items if varname is empty and thiz == INone" in {
    val f = fixture
    val node = new QVariable("")
    val results = node.matches(f.network, INone, QResult())
    results.map(_.item).toSet should be(f.network.items.toSet)
  }  
  
  it should "should not bind to variable in results if varname is empty" in {
    val f = fixture
    val node = new QVariable("")
    val results = node.matches(f.network, INone, QResult())
    for (result <- results) {
      result.variables.size should be(0)
    }
  }   

  it should "should match all network items with the given thiz parent if varname is empty" in {
    val f = fixture
    for (relation <- f.network.relations) {
      val thiz = relation.thiz
      val node = new QVariable("")
      val items = f.network.relations.filter(r => r.thiz == thiz).map(_.that).toSet
      val results = node.matches(f.network, thiz, QResult())
      results.map(_.item).toSet should be(items)
    }
  }  
  
  it should "should match all network items if thiz == INone" in {
    val f = fixture
    val node = new QVariable("Variable")
    val results = node.matches(f.network, INone, QResult())
    results.map(_.item).toSet should be(f.network.items.toSet)
  }  
  
  it should "should bind to variable in results if variable name is not empty" in {
    val f = fixture
    val node = new QVariable("Variable")
    val results = node.matches(f.network, INone, QResult())
    val variableItems = results.map(_.variable("Variable").get).toSet
    variableItems should be(f.network.items.toSet)
  } 
  
  it should "should match all network items with the given thiz parent" in {
    val f = fixture
    for (relation <- f.network.relations) {
      val thiz = relation.thiz
      val node = new QVariable("Variable")
      val items = f.network.relations.filter(r => r.thiz == thiz).map(_.that).toSet
      val results = node.matches(f.network, thiz, QResult())
      val variableItems = results.map(_.variable("Variable").get).toSet
      variableItems should be(items)
    }
  }  
  
  it should "should match only the item it is bound to" in {
    val f = fixture
    val node = new QVariable("Variable")
    val result = QResult().bind("Variable", f.iItem2)
    val results = node.matches(f.network, INone, result).toSeq
    results.size should be(1)
    results(0).item should be(f.iItem2)
  }    
  
  it should "should match only the item it is bound to with the given thiz parent" in {
    val f = fixture
    val node = new QVariable("Variable")
    val result = QResult().bind("Variable", f.iItem2)
    var results = node.matches(f.network, f.iItem1, result).toSeq
    results.size should be(1)
    results(0).item should be(f.iItem2)
    results = node.matches(f.network, f.iItem2, result).toSeq
    results.size should be(0)
  }    
}