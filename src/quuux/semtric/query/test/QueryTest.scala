package quuux.semtric.query.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.query.Query

class QueryTest extends FlatSpec with Matchers {
  import quuux.semtric.io.importer.ImporterNetworkSimple
  val relations = """Contacts|Contact Contact|Name|Fred Contact|Age|35
                       Contacts|Contact Contact|Name|Ann Contact|Phone|0314 Contact|Age|23"""
  val network = ImporterNetworkSimple.fromString(relations)
  val fred = Query("Name Fred", network).next().item
  val ann = Query("Name Ann", network).next().item
  val phone = Query("""Phone '0314'""", network).next().item

  "Relation" should "have Fred or Ann as result" in {
    val query = "Contact <Name*>"
    val results = Query(query, network)
    results.next().variables should be(Map("Name" -> fred))
    results.next().variables should be(Map("Name" -> ann))
    results.hasNext should be(false)
  }

  "And" should "have Ann with Phone as result" in {
    val query = "Contact (<Name*> & <Phone*>)"
    val results = Query(query, network)
    results.next().variables should be(Map("Name" -> ann, "Phone" -> phone))
    results.hasNext should be(false)
  }

  "Or" should "have Fred or Ann or Phone as result" in {
    val query = "Contact (<Name*> | <Phone*>)"
    val results = Query(query, network)
    results.next().variables should be(Map("Name" -> fred))
    results.next().variables should be(Map("Name" -> ann))
    results.next().variables should be(Map("Phone" -> phone))
    results.hasNext should be(false)
  }

  "InclusiveOr" should "have Fred or Ann with Phone as result" in {
    val query = "Contact (<Name*> ; <Phone*>)"
    val results = Query(query, network)
    results.next().variables should be(Map("Name" -> fred))
    results.next().variables should be(Map("Name" -> ann, "Phone" -> phone))
    results.hasNext should be(false)
  }

  "Difference" should "have Ann as result" in {
    val query = "Contact (<Name*> - <Name> & Phone)"
    val results = Query(query, network)
    results.next().variables should be(Map("Name" -> ann))
    results.hasNext should be(false)
  }
}