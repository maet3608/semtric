package quuux.semtric.io.writer

import quuux.semtric.Network
import java.io.OutputStream
import java.util.UUID
import java.io.DataOutputStream
import java.io.BufferedOutputStream
import quuux.semtric.io.Binary

/**
 * Writes a network in a binary format.
 */
class NetworkWriterBinary(compressed: Boolean, password: String)
  extends NetworkWriter(compressed, password) {
  val format = Binary

  def this() = this(false, "")

  private def unsupportedOperationError() = {
    throw new UnsupportedOperationException(
      "Binary writer cannot write to strings or console but to byte arrays!")
  }

  // Cannot write binary data to string. Throw exception.
  override def writeToString(network: Network): String = unsupportedOperationError()

  // Should not write binary data to console. Throw exception.
  override def writeToConsole(network: Network): Unit = unsupportedOperationError()

  def write(network: Network, os: OutputStream) {
    val dos = new DataOutputStream(new BufferedOutputStream(os))

    def writeUUID(uuidString: String) {
      val uuid = UUID.fromString(uuidString)
      dos.writeLong(uuid.getMostSignificantBits)
      dos.writeLong(uuid.getLeastSignificantBits)
    }

    val nItems = network.items.size
    val nRelations = network.relations.size
    dos.writeInt(nItems)
    dos.writeInt(nRelations)
    progress.start(nItems + nRelations)
    
    for (item <- network.items) {
      progress.increment()
      writeUUID(item.uuid)
      dos.writeUTF(item.kind)
      dos.writeUTF(item.value.toString)
    }
    
    for (Relation <- network.relations) {
      writeUUID(Relation.thiz.uuid)
      writeUUID(Relation.that.uuid)
    }
    
    progress.end()
    dos.close()
  }
}

object NetworkWriterBinary {
  def apply(compressed: Boolean, password: String) = new NetworkWriterBinary(compressed, password)
}