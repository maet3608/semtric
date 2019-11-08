package quuux.semtric

import quuux.semtric.io.writer._
import quuux.semtric.io.reader._
import quuux.semtric.Item._
import quuux.semtric.Relation._

/**
 * Enables an object to be written to the storage.
 */
trait StorageWritable {
  def write(storage: Storage, to: AnyItem): Unit
}

/**
 * Enables an object to be read from the storage.
 */
trait StorageReadable[T] {
  def read(storage: Storage, from: AnyItem): T
}

/**
 * Persists objects that are StorageWritable and StorageReadable as
 * a semantic network.
 */
class Storage(private var network: Network, val root: AnyItem) {
  var relations = List[AnyRelation]()

  private def error(message: String) = throw new IllegalArgumentException(message)

  /** Writes a group with the given name under the to group to the storage. Returns the group. */
  def writeGroup(to: AnyItem, name: AnyItem): AnyItem = {
    relations = Relation(to, name)::relations
    name
  }

  /** Writes a value to the group with the given name which must be located under the to group. */
  def writeValue(to: AnyItem, name: String, value: AnyItem) {
    val item = writeGroup(to, name)
    writeGroup(item, value)
  }

  /** Same as above but writes multiple values. Order of values is not preserved! */
  def writeValues(to: AnyItem, name: String, values: Iterable[AnyItem]) {
    val item = writeGroup(to, name)
    values.foreach(writeGroup(item, _))
  }

  private var isWritten = Map[Any, AnyItem]()
  private def writeStoragable(group: AnyItem, obj: StorageWritable) {
    obj.write(this, group)
  }

  /** Writes a storagable object under the given group name to the to group. */
  def writeStoragable(to: AnyItem, name: String, obj: StorageWritable) {
    if (isWritten.contains(obj)) {
      val group = isWritten(obj)
      if (group.value != name) {
        error("Object: " + obj + " with name '" + name +
          "' already written under name '" + group.value + "'")
      }
      relations = Relation(to, group)::relations
    } else {
      val group = writeGroup(to, name)
      writeStoragable(group, obj)
      isWritten += obj -> group
    }
  }

  /** Same as above but writes multiple storagable objects. Order of objects is not preserved! */
  def writeStoragables(to: AnyItem, name: String, objects: Iterable[StorageWritable]) {
    objects.foreach(writeStoragable(to, name, _))
  }

  /**
   * Reads a group with the given name from the from group. Default is returned if a group
   * with the given name does not exist and default is not null. If default is null
   * an IllegalArgumentException is thrown.
   */
  def readGroup(from: AnyItem, name: String, default: AnyItem = null): AnyItem = {
    val items = network.thats(from, name)
    if (items.isEmpty) {
      if (default == null) error(
        "Can't read group '" + name + "' from '" + from + "'  uuid="+from.uuid)
      return default
    }
    items.next()
  }

  /** As above but returns all groups with the given name under the from group. */
  def readGroups(from: AnyItem, name: String): Iterable[AnyItem] =
    network.thats(from, name).toIterable

  /** Returns all groups under the from group. */
  def readGroups(from: AnyItem): Iterable[AnyItem] =
    network.thats(from).toIterable

  /**
   * Reads a value from the group with the given name under the from group. Fails
   * if group does not exist.
   */
  def readValue[T](from: AnyItem, name: String): T = {
    val item = readGroup(from, name)
    val thats = network.thats(item)
    if (thats.isEmpty) error("Can't read value for '" + name + "' from '" + from + "'")
    thats.next().value.asInstanceOf[T]
  }

  /**
   * Reads a value from the group with the given name under the from group. Default is
   * returned if group does not exist.
   */
  def readValue[T](from: AnyItem, name: String, default: T): T = {
    val item = readGroup(from, name, INone)
    if (item == INone) return default
    val items = network.thats(item)
    if (items.isEmpty) default else items.next().value.asInstanceOf[T]
  }

  /** As above but returns all values under the group with the given name. */
  def readValues[T](from: AnyItem, name: String): Iterable[T] = {
    val item = readGroup(from, name)
    network.thats(item).map(_.value.asInstanceOf[T]).toIterable
  }

  /** Reads all values under the from group. */
  def readValues[T](from: AnyItem): Iterable[T] = {
    network.thats(from).map(_.value.asInstanceOf[T]).toIterable
  }

  private var isRead = Map[AnyItem, Any]()
  private def readStoragable[T](group: AnyItem, obj: StorageReadable[T]): T = {
    if (!isRead.contains(group)) { isRead += group -> obj.read(this, group) }
    isRead(group).asInstanceOf[T]
  }

  /**
   * Reads a storagable object from the group with given name under the from group.
   * The provided obj must be a factory for the object.
   */
  def readStoragable[T](from: AnyItem, name: String, obj: StorageReadable[T]): T =
    readStoragable(readGroup(from, name), obj)

  /** As above but reads multiple storagable objects. */
  def readStoragables[T](from: AnyItem, name: String, obj: StorageReadable[T]): Iterable[T] = {
    network.thats(from, name).map(readStoragable(_, obj)).toIterable
  }

  /** Commits data written to the storage, which then can be read. */
  def commit() {
    network = Network(relations.reverse.toVector)
  }

  /**
   * Saves the storage to the given file.
   *  If the file is encrypted the getPassword function will be called
   *  to get the password necessary to open decrypt the file.
   *  The getProgress function will be called during saving in not null.
   */
  def save(filepath: String, binary: Boolean = false, compressed: Boolean = false,
           getPassword: => String = "", getProgress: Double => Unit = null) {
    commit()
    val writer = NetworkWriter(binary, compressed, getPassword)
    writer.setProgressListener(getProgress)
    writer.writeToFile(network, filepath)
  }

  /** Prints the storage content as a tree */
  def print(showUUID: Int = 0, showKind: Boolean = false, maxLevel: Int = 0, maxLines: Int = 0) =
    network.print(showUUID, showKind, maxLevel, maxLines)
}

/**
 * Storage factory.
 */
object Storage {
  private val ROOT = "SEMTRIC_STORAGE_ROOT"

  /** Creates an empty storage. */
  def apply() = new Storage(Network(), IString(ROOT))

  /** Loads a storage from the given file. */
  def load(filepath: String, getPassword: => String = "", getProgress: Double => Unit = null) = {
    val reader = NetworkReader(filepath, getPassword)
    reader.setProgressListener(getProgress)
    val network = reader.readFromFile(filepath)
    val root = network.items(ROOT).next()
    new Storage(network, root)
  }

  /** Saves a writable to the given file. */
  def save(obj: StorageWritable, filepath: String, binary: Boolean = false,
           compressed: Boolean = false, getPassword: => String = "",
           getProgress: Double => Unit = null) {
    val storage = Storage()
    obj.write(storage, storage.root)
    storage.save(filepath, binary, compressed, getPassword, getProgress)
  }

}

