package quuux.semtric.io.importer

import quuux.semtric.Network
import quuux.semtric._
import quuux.semtric.Item._
import quuux.semtric.Relation._
import java.io.InputStream
import java.io.FileInputStream
import java.io.File
import java.nio.charset.StandardCharsets
import java.io.ByteArrayInputStream

/*
 * Importers support the import of networks described in different data formats such as
 * general network descriptions (e.g. in Json, XML) or application specific networks
 * (e.g. Japanese Kanji).
 * Importers contain an empty set of relations by default that can be filled with
 * the provided methods.
 */
abstract class Importer {
  protected var relations = List[AnyRelation]()

  /** To be implemented by derived class. */
  def fromStream(is: InputStream): Network

  // Read from various input formats such as files, strings and byte arrays.
  def fromFile(filepath: String): Network =
    fromFile(new File(filepath))
  def fromFile(file: File): Network =
    fromStream(new FileInputStream(file))
  def fromString(string: String): Network =
    fromByteArray(string.getBytes(StandardCharsets.UTF_8))
  def fromByteArray(array: Array[Byte]): Network =
    fromStream(new ByteArrayInputStream(array))

  /** Adds a relation to relations and returns that. Thiz is parent of that. */
  def addRelation(thiz: AnyItem, that: AnyItem): AnyItem = {
    relations = Relation(thiz, that) :: relations
    that
  }

  /** Creates items for values and relates them to thiz. All values have thiz as parent. */
  def addRelations(thiz: AnyItem, values: String*): Seq[AnyItem] = {
    values.map(addRelation(thiz, _))
  }

  /**
   * Adds a property to the relation set. Properties are represented as a chain of two relations
   *  of the following form: thiz-property-value, e.g. car-color-blue.
   */
  def addProperty(thiz: AnyItem, property: String, value: AnyItem) {
    addRelation(addRelation(thiz, IString(property)), value)
  }

  // Creates the network from the added relations.
  def createNetwork() = Network(relations.reverse.toVector)
}
