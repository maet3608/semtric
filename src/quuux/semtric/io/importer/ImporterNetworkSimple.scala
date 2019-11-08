package quuux.semtric.io.importer

import scala.io.Source
import scala.collection.mutable.AnyRefMap
import java.io.InputStream
import quuux.semtric.Network
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._

/**
 * Importer for simple descriptions of networks largely to construct networks for test purposes.
 * A network is described by a sequence of relations separated by white spaces. A relation has a
 * left and a right side item separated by '|', e.g. 'Name|Fred'. Items can have a 
 * type annotation given by colon sign and the type name, e.g. 'Age|23:int'.
 *
 * Example:
 * Contacts|Contact Contact|Name|Fred
 * Contacts|Contact Contact|Name|Ann Contact|Age|23:Int
 * 
 * This represents the following tree:
 * Contacts
 * - Contact
 *   - Name
 *     - Fred
 * - Contact
 *   - Name
 *     - Ann
 *   - Age
 *     - 23 
 *  
 * The same network could have been written (without chaining) as:
 * Contacts|Contact Contact|Name Name|Fred
 * Contacts|Contact Contact|Name Name|Ann Contact|Age Age|23:Int        
 *
 * Notes:
 * - Some other type annotations are: pi|3.14:real, DOB:string|2015-20-09:datetime
 * - Relations can be chained, e..g Contact|Name|Fred to describe properties.
 * - Items with the same value (e.g. Contact) are referenced when on the left side
 *   of a relation and created newly when on the right side (or if no reference exists), see:
 *   Contacts|Contact Contact|Name
 *   That means the 'Contact' item for Fred is different from that of Ann but
 *   'Contacts' is the same.
 */
object ImporterNetworkSimple extends Importer {

  def fromStream(is: InputStream): Network = {
    val text = Source.fromInputStream(is)("UTF-8").mkString
    var value2item = AnyRefMap[String, AnyItem]()

    def exists(thiz: AnyItem, value: String): Boolean = {
      for (r <- relations)
        if (r.thiz.value == thiz.value && r.that.value == value) return true
      return false
    }

    def item(thiz: AnyItem, description: String): AnyItem = {
      val desc = """([^:]+)(:.+)?""".r
      val desc(value, kind) = description
      if (!value2item.contains(value) || exists(thiz, value)) {
        val item = Item(Item.newUUID, if (kind == null) IString.kind else kind.drop(1), value)
        value2item.update(value, item)
      }
      value2item(value)
    }

    for (element <- text.split("\\s+")) {
      var that: AnyItem = INone
      for (Array(thizValue, thatValue) <- element.split("\\|").sliding(2)) {
        val thiz = item(that, thizValue)
        that = item(thiz, thatValue)
        addRelation(thiz, that)
      }
    }

    is.close()
    createNetwork()
  }

}