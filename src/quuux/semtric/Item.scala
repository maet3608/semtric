package quuux.semtric

import Item._
import java.net.URI
import hirondelle.date4j.DateTime

/**
 * Items have a unique id, a kind and a value.
 * The kind of an item is a string descriptor of the item type, e.g. "int" or "bool" for primitive
 * types and otherwise a mimetype.
 * audio/mpeg are the nodes in the network linked by relations.
 * There are different item types depending on their value type, e.g. IInt, IString items.
 * Items are immutable but a copy constructor is provided.
 *
 * When adding new item types make sure to extend the tests in ItemTest and add an item
 * of the new type to the test networks in ReaderWriterTest.
 */
sealed abstract class Item[T](val uuid: Item.UUID, val kind: String, val value: T) {  
    
  /** Returns -1 if this < item, +1 if this > item and 0 if this==item */
  def compare(item:AnyItem):Int 
    
  /** Copy constructor. */
  def copy(value: T): Item[T]

  /** Items are equal if their UUIDs are equal. */
  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case i: AnyItem             => this.uuid == i.uuid
      case _                      => false
    }
  }

  override def hashCode(): Int = uuid.hashCode()

  /** Returns a string of the form uuid:kind:value */
  def description = uuid + ":" + kind + ": " + value

  /** Returns value of item as string. */
  override def toString() = value.toString()
}

object Item {
  type UUID = String
  type AnyItem = Item[_]

  /** Creates new UUID. */
  def newUUID = java.util.UUID.randomUUID().toString

  /** A fixed UUID for none objects. */
  val NONE_UUID = new java.util.UUID(0, 0).toString
  
  /** Default comparator: string based comparison. */
  def compare(item1:AnyItem, item2:AnyItem) = 
    item1.value.toString compare item2.value.toString

  // Implicit conversions.
  implicit def toStringItem(value: String): AnyItem = IString(value)
  implicit def toIntItem(value: Int): AnyItem = IInt(value)
  implicit def toLongItem(value: Long): AnyItem = ILong(value)
  implicit def toRealItem(value: Double): AnyItem = IReal(value)
  implicit def toRealItem2(value: Float): AnyItem = IReal(value)
  implicit def toBoolItem(value: Boolean): AnyItem = IBool(value)
  implicit def toDateTimeItem(value: DateTime): AnyItem = IDateTime(value)   
  //implicit def toUUIDItem(value: UUID): AnyItem = IUUID(value)  // UUIDs are strings
  
  
  /** Item factory */
  def apply(uuid: String, kind: String, value: String): AnyItem = {
    kind match {
      case INone.kind     => INone
      case IString.kind   => new IString(uuid, value)
      case IBool.kind     => new IBool(uuid, value.toBoolean)
      case IInt.kind      => new IInt(uuid, value.toInt)
      case ILong.kind     => new ILong(uuid, value.toLong)
      case IReal.kind     => new IReal(uuid, value.toDouble)
      case IDateTime.kind => new IDateTime(uuid, new DateTime(value))
      case IUUID.kind     => new IUUID(uuid, value)      
      case _              => new IURI(uuid, kind, new URI(value))
    }
  }

  def apply(kind: String, value: String): AnyItem = Item(newUUID, kind, value)

}

object INone extends Item(NONE_UUID, "none", "NONE") {
  def compare(item:AnyItem):Int = -1
  def copy(value: String) = throw new UnsupportedOperationException("Can't copy NONE")
}

// Strings must contain only printable characters if stored in text format (NetworkWriterDefault). 
class IString(uuid: UUID, value: String)
  extends Item(uuid, IString.kind, value.intern()) {
  def compare(item:AnyItem):Int = Item.compare(this, item)
  def copy(value: String) = IString(value)
}

object IString {
  val kind = "string"
  def apply(value: String) = new IString(newUUID, value)
}

class IBool(uuid: UUID, value: Boolean)
  extends Item(uuid, IBool.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:Boolean => this.value compare thatValue
    case _ => Item.compare(this, item)
  }
  def copy(value: Boolean) = IBool(value)
}

object IBool {
  val kind = "bool"
  def apply(value: Boolean) = new IBool(newUUID, value)
  def apply(value: String) = new IBool(newUUID, value == "true")
}

class IInt(uuid: UUID, value: Int)
  extends Item(uuid, IInt.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:Int => this.value compare thatValue
    case thatValue:Double => this.value.toDouble compare thatValue
    case _ => Item.compare(this, item)
  }  
  def copy(value: Int) = IInt(value)
}

object IInt {
  val kind = "int"
  def apply(value: Int) = new IInt(newUUID, value)
}

class ILong(uuid: UUID, value: Long)
  extends Item(uuid, ILong.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:Long => this.value compare thatValue
    case thatValue:Double => this.value.toDouble compare thatValue
    case _ => Item.compare(this, item)
  }  
  def copy(value: Long) = ILong(value)
}

object ILong {
  val kind = "long"
  def apply(value: Long) = new ILong(newUUID, value)
}

class IReal(uuid: UUID, value: Double)
  extends Item(uuid, IReal.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:Int => this.value compare thatValue.toDouble
    case thatValue:Double => this.value compare thatValue
    case _ => Item.compare(this, item)
  }    
  def copy(value: Double) = IReal(value)
}

object IReal {
  val kind = "real"
  def apply(value: Double) = new IReal(newUUID, value)
}

class IDateTime(uuid: UUID, value: DateTime)
  extends Item(uuid, IDateTime.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:DateTime => this.value compareTo thatValue
    case _ => Item.compare(this, item)
  }
  def copy(value: DateTime) = IDateTime(value)
  def copy(value: String) = IDateTime(value)
}

object IDateTime {
  val kind = "datetime"
  def apply(value: DateTime) = new IDateTime(newUUID, value)
  def apply(value: String) = new IDateTime(newUUID, new DateTime(value))
}

class IUUID(uuid: UUID, value: UUID)
  extends Item(uuid, IUUID.kind, value) {
  def compare(item:AnyItem):Int = item.value match {
    case thatValue:UUID => this.value compareTo thatValue
    case _ => Item.compare(this, item)
  }
  def copy(value: UUID) = IUUID(value)
}

object IUUID {
  val kind = "uuid"
  def apply(value: UUID) = new IUUID(newUUID, value)
}

class IURI(uuid: UUID, mimetype: String, value: URI)
  extends Item(uuid, mimetype, value) {
  def compare(item:AnyItem):Int = Item.compare(this, item)
  def copy(value: URI) = IURI(mimetype, value)
  def copy(value: String) = IURI(mimetype, value)
}

object IURI {
  import java.net.URI
  def apply(mimetype: String, value: URI): IURI = new IURI(newUUID, mimetype, value)
  def apply(mimetype: String, value: String): IURI = IURI(mimetype, new URI(value))
}
