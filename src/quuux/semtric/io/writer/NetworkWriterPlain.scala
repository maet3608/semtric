package quuux.semtric.io.writer

import quuux.semtric.Network
import quuux.semtric.io.NetworkIO.encodeNewline
import java.io.OutputStream
import java.io.PrintStream
import java.io.OutputStreamWriter
import java.io.BufferedWriter
import quuux.semtric.io.Plain

/**
 * Writes a network in plain format, which is a sequence of items and then relations.
 * Each item is written in three lines
 * - uuid
 * - kind
 * - value
 * and each relation is written in two lines
 * - thiz.uuid
 * - that.uuid
 * The numbers of items and relations are written before each block of items or relations.
 * Multiline values (multi-line strings) are encoded (newline replaced) to be written in one line.
 */
class NetworkWriterPlain(compressed: Boolean, password: String)
  extends NetworkWriter(compressed, password) {
  val format = Plain

  def this() = this(false, "")
  
  private def unsupportedOperationError() = {
    throw new UnsupportedOperationException(
      "Cannot write encrypted or compressed to strings or console but to byte arrays!")
  }

  // Cannot write encrypted or compressed data to string. Throw exception.
  override def writeToString(network: Network): String = {
    if (compressed || !password.isEmpty()) unsupportedOperationError()
    super.writeToString(network)
  }

  // Should not write encrypted or compressed data to console. Throw exception.
  override def writeToConsole(network: Network): Unit = {
    if (compressed || !password.isEmpty()) unsupportedOperationError()
    super.writeToConsole(network)
  } 

  def write(network: Network, os: OutputStream) {
    val writer = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"));

    def writeln(string: String) {
      writer.write(string)
      writer.write("\n")
    }

    val nItems = network.items.size
    val nRelations = network.relations.size
    writeln(nItems.toString)
    writeln(nRelations.toString)
    progress.start(nItems + nRelations)
    
    for (item <- network.items) {
      progress.increment()
      writeln(item.uuid.toString)
      writeln(item.kind)
      writeln(encodeNewline(item.value))
    }
    
    for (Relation <- network.relations) {
      progress.increment()
      writeln(Relation.thiz.uuid.toString)
      writeln(Relation.that.uuid.toString)
    }
    
    progress.end()
    writer.close()
  }
}

object NetworkWriterPlain {
  def apply(compressed: Boolean, password: String) = new NetworkWriterPlain(compressed, password)
}