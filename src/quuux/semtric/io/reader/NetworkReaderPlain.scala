package quuux.semtric.io.reader

import java.io.BufferedReader
import scala.io.Source
import scala.collection.mutable.AnyRefMap
import quuux.semtric.Network
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import quuux.semtric.io.NetworkIO.decodeNewline
import java.io.InputStream
import quuux.semtric.io.Plain

/**
 * Reads networks in plain format.
 */
class NetworkReaderPlain(compressed: Boolean, password: String)
  extends NetworkReader(compressed, password) {
  val format = Plain

  def this() = this(false, "")

  private def unsupportedOperationError() = {
    throw new UnsupportedOperationException(
      "Cannot read compressed or encryped from strings but from byte arrays!")
  }

  // Cannot read encrypted or compressed data from string. Throw exception.
  override def readFromString(string: String): Network = {
    if (compressed || !password.isEmpty()) unsupportedOperationError
    super.readFromString(string)
  }

  def read(is: InputStream): Network = {
    val lines = Source.fromInputStream(is)("UTF-8").getLines()

    val nItems = lines.next.toInt
    val nRelations = lines.next.toInt
    progress.start(nItems + nRelations)

    var uuid2item = AnyRefMap[String, AnyItem]()
    var items = List[AnyItem]()   // In addition to uuid2item to preserve order of loaded items.
    for (_ <- 0 until nItems) {
      progress.increment()
      val (uuid, kind, value) = (lines.next, lines.next, lines.next)
      val item = Item(uuid, kind, decodeNewline(value))
      uuid2item += (uuid -> item)
      items = item::items
    }

    var relations = List[AnyRelation]()
    for (_ <- 0 until nRelations) {
      progress.increment()
      val relation = new Relation(uuid2item(lines.next), uuid2item(lines.next))
      relations = relation::relations
    }

    progress.end()
    is.close()
    Network(items.reverse.toVector, relations.reverse.toVector)
  }
}

object NetworkReaderPlain {
  def apply(compressed: Boolean, password: String) = new NetworkReaderPlain(compressed, password)
}