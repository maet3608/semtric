package quuux.semtric.io

import java.io.InputStream
import java.io.OutputStream
import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import java.io.ByteArrayInputStream
import java.io.File

/** Network file formats. */
sealed abstract class Format(val id: Byte, val name: String) {
  override def toString() = name
}

object Unknown extends Format(0, "Unknown")
object Plain extends Format(1, "Plain")
object Binary extends Format(2, "Binary")

/** Format factory. */
object Format {
  /** Creates format for the given id. */
  def apply(id: Byte) = id match {
    case 0 => Unknown
    case 1 => Plain
    case 2 => Binary
    case _ => throw new IllegalArgumentException("Unknown network format id: " + id)
  }
}



/** Describes the header of a network file. */
class Header(val format: Format, val compressed: Boolean, val encrypted: Boolean) {

  /** Checks if header format is same as given format. Throws exception otherwise. */
  def check(other: Format) {
    if (format != other) {
      throw new IllegalArgumentException(
        "Network to read is in wrong format. Expect:" + format + " but got:" + other)
    }
  }

  override def equals(other: Any): Boolean = {
    def isSame(h: Header) =
      this.format == h.format && this.compressed == h.compressed && this.encrypted == h.encrypted
    other match {
      case a: AnyRef if this eq a => true
      case h: Header              => isSame(h)
      case _                      => false
    }
  }

  override def hashCode(): Int =
    format.hashCode() * (if (compressed) 1 else 7) * (if (encrypted) 3 else 5)
}

/** Header factory. */
object Header {
  /** Magic number to identify Semtric files. */
  private val MAGIC: Byte = 5

  /** Creates a new header. */
  def apply(format: Format, compressed: Boolean, encrypted: Boolean) =
    new Header(format, compressed, encrypted)

  // Reads the header from various input formats such as files, strings and byte arrays.
  def readFromFile(filepath: String): Header =
    readFromStream(new FileInputStream(filepath))
  def readFromFile(file: File): Header =
    readFromStream(new FileInputStream(file))
  def readFromString(string: String): Header =
    readFromByteArray(string.getBytes(StandardCharsets.UTF_8))
  def readFromByteArray(array: Array[Byte]): Header =
    readFromStream(new ByteArrayInputStream(array))
  def readFromStream(is: InputStream): Header = Header.read(is)

  /** Reads an header from the given input stream */
  def read(is: InputStream): Header = {
    val bytes = Array[Byte](0, 0, 0, 0, 0, 0, 0) // currently expect fixed length header.
    val length = is.read(bytes)
    if (length != bytes.length || bytes(1) != MAGIC) return new Header(Unknown, false, false)
    val Array(len, magic, formatId, compressed, encryped, reserved1, reserved2) = bytes
    Header(Format(formatId), compressed != 0, encryped != 0)
  }

  /** Writes an header to the given input stream.  */
  def write(os: OutputStream, format: Format, compressed: Boolean, encrypted: Boolean) {
    val bytes = Array[Byte](0, 0, 0, 0, 0, 0, 0)
    bytes(0) = 7 // length of header. Currently fixed but could be changed dynamically.
    bytes(1) = MAGIC
    bytes(2) = format.id
    bytes(3) = if (compressed) 1 else 0
    bytes(4) = if (encrypted) 1 else 0
    // bytes(5..6) are reserved.
    os.write(bytes)
  }
}
