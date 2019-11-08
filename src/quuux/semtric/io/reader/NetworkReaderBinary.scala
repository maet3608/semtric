package quuux.semtric.io.reader

import scala.collection.mutable.Map
import scala.collection.mutable.AnyRefMap
import quuux.semtric.Network
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import java.util.UUID
import java.io.InputStream
import java.io.DataInputStream
import java.io.BufferedInputStream
import quuux.semtric.io.Binary

/**
 * Reads networks in binary format.
 */
class NetworkReaderBinary(compressed: Boolean, password: String)
  extends NetworkReader(compressed, password) {
  val format = Binary

  def this() = this(false, "");

  private def formatError() = {
    throw new IllegalArgumentException("Not in correct format. Expect '" + format + "'")
  }

  private def unsupportedOperationError() = {
    throw new UnsupportedOperationException(
      "Binary reader cannot read from strings but from byte arrays!")
  }

  // Cannot read binary data from string. Throw exception.
  override def readFromString(string: String): Network = unsupportedOperationError()

  def read(is: InputStream): Network = {
    val dis = new DataInputStream(new BufferedInputStream(is))

    def readUUID(): String = {
      val mostSigBits = dis.readLong()
      val leastSigBits = dis.readLong()
      new UUID(mostSigBits, leastSigBits).toString
    }

    val nItems = dis.readInt()
    val nRelations = dis.readInt()
    progress.start(nItems + nRelations)
    
    var uuid2item = AnyRefMap[String, AnyItem]()
    var items = List[AnyItem]()  // In addition to uuid2item to preserve order of loaded items.
    for (_ <- 0 until nItems) {
      progress.increment()
      val uuid = readUUID()
      val kind = dis.readUTF()
      val value = dis.readUTF()
      val item = Item(uuid, kind, value)
      uuid2item += (uuid -> item)
      items = item::items
    }
    
    var relations = List[AnyRelation]()
    for (_ <- 0 until nRelations) {
      progress.increment()
      val relation = new Relation(uuid2item(readUUID()), uuid2item(readUUID()))
      relations = relation::relations
    }
    
    progress.end()
    dis.close()
    Network(items.reverse.toVector, relations.reverse.toVector)
  }
}

object NetworkReaderBinary {
  def apply(compressed: Boolean, password: String) = new NetworkReaderBinary(compressed, password)
}