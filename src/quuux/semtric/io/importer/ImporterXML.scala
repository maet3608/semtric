package quuux.semtric.io.importer

import scala.xml.NodeSeq
import scala.xml.Node

/**
 * Extends Importers by methods specific to the import of XML data.
 */
abstract class ImporterXML extends Importer {

  /** Returns true if an XML node has an attribute with the given name. */
  def hasAttrib(node: Node, name: String) = node.attributes.exists(_.value.text == name)

}