package quuux.semtric.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Item._
import hirondelle.date4j.DateTime
import java.net.URI

class ItemTest extends FlatSpec with Matchers {

  "None item" should "be singleton" in {
    assert(INone.hashCode() == INone.hashCode())
  }
  
  it should "throw UnsupportedOperationException when copied" in {
    a[UnsupportedOperationException] should be thrownBy {
      INone.copy("")
    }    
  }  

  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    // uuid and value are ignored for None items.
    val item = Item(uuid, INone.kind, "foo")
    assert(item == INone)
    assert(item.uuid != uuid)     // None has fixed uuid.
    assert(item.value == "NONE")  // None has fixed value.   
  }
  
  "Item" should "have unique identifiers" in {
    val item1 = IString("str")
    val item2 = IString("str")
    assert(item1.uuid != item2.uuid)
  }
  
  it should "correctly perform implicit conversions" in {
    def identity(item: AnyItem): AnyItem = item
    assert(identity("String").isInstanceOf[IString])
    assert(identity(12).isInstanceOf[IInt])
    assert(identity(10000000l).isInstanceOf[ILong])
    assert(identity(3.14).isInstanceOf[IReal])
    assert(identity(3.14f).isInstanceOf[IReal])
    assert(identity(true).isInstanceOf[IBool])   
    assert(identity(new DateTime("30.01.2015")).isInstanceOf[IDateTime])
  }
    
  it should "create different items for implicit conversion" in {  
    def identity(item: AnyItem): AnyItem = item
    val value = "String"
    assert(identity(value) != identity(value))
    
    // Identity for the same item of course works.
    val item:AnyItem = "String"  // implicit conversion here.
    assert(item.isInstanceOf[IString])
    assert(identity(item) == identity(item))   // No conversion there.
  }  
    
  "String item" should "store string" in {
    val item = IString("str")
    item.value should be("str")
  }

  it should "should have kind" in {
    val item = IString("str")
    item.kind should be(IString.kind)
  }

  it should "should have copy constructor for string" in {
    val item1 = IString("str1")
    val item2 = item1.copy("str2")
    item2.value should be("str2")
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IString.kind, "some text")
    assert(item.isInstanceOf[IString])
    assert(item.uuid == uuid)
    assert(item.value == "some text")
  }  
  
  "Bool item" should "store boolean" in {
    val item = IBool(true)
    item.value should be(true)
  }

  it should "should have kind" in {
    val item = IBool(true)
    item.kind should be(IBool.kind)
  }

  it should "should have copy constructor for boolean" in {
    val item1 = IBool(true)
    val item2 = item1.copy(false)
    item2.value should be(false)
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IBool.kind, true.toString)
    assert(item.isInstanceOf[IBool])
    assert(item.uuid == uuid)
    assert(item.value == true)
  }   
  

  "Int item" should "store int" in {
    val item = IInt(7)
    item.value should be(7)
  }

  it should "should have kind" in {
    val item = IInt(7)
    item.kind should be(IInt.kind)
  }

  it should "should have copy constructor for int" in {
    val item1 = IInt(1)
    val item2 = item1.copy(2)
    item2.value should be(2)
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IInt.kind, 123.toString)
    assert(item.isInstanceOf[IInt])
    assert(item.uuid == uuid)
    assert(item.value == 123)
  }   

  "Long item" should "store long" in {
    val item = ILong(7)
    item.value should be(7)
  }

  it should "should have kind" in {
    val item = ILong(7)
    item.kind should be(ILong.kind)
  }

  it should "should have copy constructor for long" in {
    val item1 = ILong(1)
    val item2 = item1.copy(2)
    item2.value should be(2)
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, ILong.kind, 123.toString)
    assert(item.isInstanceOf[ILong])
    assert(item.uuid == uuid)
    assert(item.value == 123)
  }  
  
  "Real item" should "store double" in {
    val item = IReal(3.14)
    item.value should be(3.14)
  }

  it should "should have kind" in {
    val item = IReal(3.14)
    item.kind should be(IReal.kind)
  }

  it should "should have copy constructor for double" in {
    val item1 = IReal(1.1)
    val item2 = item1.copy(1.2)
    item2.value should be(1.2)
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IReal.kind, 0.0123.toString)
    assert(item.isInstanceOf[IReal])
    assert(item.uuid == uuid)
    assert(item.value == 0.0123)
  }    

  "DateTime item" should "store DateTime" in {
    val item = IDateTime("2014-09-20 11:13")
    item.value should be(new DateTime("2014-09-20 11:13"))
    item.value.plusDays(2) should be(new DateTime("2014-09-22 11:13"))
  }

  it should "should have kind" in {
    val item = IDateTime("2014-09-20 11:13")
    item.kind should be(IDateTime.kind)
  }

  it should "should have copy constructor for DateTime" in {
    val item1 = IDateTime("2014-09-20 11:13")
    val item2 = item1.copy("2015-10-21 12:14")
    item2.value should be(new DateTime("2015-10-21 12:14"))
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IDateTime.kind, "2015-10-21 12:14")
    assert(item.isInstanceOf[IDateTime])
    assert(item.uuid == uuid)
    assert(item.value == new DateTime("2015-10-21 12:14"))
  }  
  
  "UUID item" should "store UUID" in {
    val item = IUUID("8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    item.value should be("8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
  }

  it should "should have kind" in {
    val item = IUUID("8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    item.kind should be(IUUID.kind)
  }

  it should "should have copy constructor for DateTime" in {
    val item1 = IUUID("8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    val item2 = item1.copy("5b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    item2.value should be("5b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }
  
  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, IUUID.kind, "8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
    assert(item.isInstanceOf[IUUID])
    assert(item.uuid == uuid)
    assert(item.value == "8b070bb2-35a8-491b-9bbd-3e65fc386bc2")
  }     

  "URI item" should "store URI" in {
    val item = IURI("audio/mp3", "file:///mysong.mp3")
    item.value should be(new URI("file:///mysong.mp3"))
  }

  it should "should have kind" in {
    val item = IURI("audio/mp3", "file:///mysong.mp3")
    item.kind should be("audio/mp3")
  }

  it should "should have copy constructor for URI" in {
    val item1 = IURI("audio/mp3", "file:///mysong.mp3")
    val item2 = item1.copy("file:///other_song.mp3")
    item2.value should be(new URI("file:///other_song.mp3"))
    assert(item1.kind == item2.kind)
    assert(item1.uuid != item2.uuid)
  }

  it should "be constructable via factory" in {
    val uuid = "4b070bb2-35a8-491b-9bbd-3e65fc386bc2"
    val item = Item(uuid, "audio/mp3", "file:///mysong.mp3")
    assert(item.isInstanceOf[IURI])
    assert(item.uuid == uuid)
    assert(item.value == new URI("file:///mysong.mp3"))
  }    
}