package quuux.semtric.io.importer.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import quuux.semtric.io.importer.ImporterNetworkSimple

class ImporterNetworkSimpleTest extends FlatSpec with Matchers {
  
  "ImporterNetworkSimple" should "create correct network" in {
    val net = """Contacts|Contact Contact|Name|Fred
                 Contacts|Contact Contact|Name|Ann Contact|Phone|0314 Contact|Age|23:int"""
    val network = ImporterNetworkSimple.fromString(net)

    network.items("Contacts").size should be(1)
    network.items("Contact").size should be(2)
    network.items("Name").size should be(2)    
    network.items("Fred").size should be(1)
    network.items("Ann").size should be(1)
    network.items("Age").size should be(1)
    network.items(23).size should be(1)
    network.items("Phone").size should be(1)
    network.items("0314").size should be(1)    
  }
  
  it should "create items of correct type" in {    
    val net = """Age|23:int pi:string|3.14:real"""
    val network = ImporterNetworkSimple.fromString(net)
    
    val itemAge = network.items("Age").toList.head
    assert(itemAge.isInstanceOf[IString])
    
    val item23 = network.items(23).toList.head
    assert(item23.isInstanceOf[IInt])
    
    val itemPi = network.items("pi").toList.head
    assert(itemPi.isInstanceOf[IString])
    
    val item314 = network.items(3.14).toList.head
    assert(item314.isInstanceOf[IReal])        
  }  

}