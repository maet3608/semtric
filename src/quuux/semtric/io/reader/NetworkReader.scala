package quuux.semtric.io.reader

import quuux.semtric.Network
import java.io.File
import java.io.InputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.io.FileInputStream
import java.util.zip.InflaterInputStream
import quuux.semtric.io.NetworkIO
import quuux.semtric.io.reader._
import quuux.semtric.io._
import quuux.semtric.utils.ProgressTracker



/**
 * Base class for network readers that requires the implementation of canRead and read in
 * derived classes and provides reading from files, strings and byte arrays.
 * Networks can be read in a compressed format and will be decrypted if a non-empty password
 * is given.
 * see NetworkIOBenchmark for a speed comparison of different readers and writers.
 */
abstract class NetworkReader(val compressed: Boolean, val password: String) extends NetworkIO {

  /** Reads a network from an input stream. */
  def read(is: InputStream): Network

  // Read from various input formats such as files, strings and byte arrays.
  def readFromFile(filepath: String): Network =
    readFromFile(new File(filepath))
  def readFromFile(file: File): Network =
    readFromStream(new FileInputStream(file))
  def readFromString(string: String): Network =
    readFromByteArray(string.getBytes(StandardCharsets.UTF_8))
  def readFromByteArray(array: Array[Byte]): Network =
    readFromStream(new ByteArrayInputStream(array))

  /** Wraps input stream in compressed and/or decrypted stream. */
  private def wrapInputStream(is: InputStream) = {
    var inputStream = is
    Header.read(is).check(format)
    if (!password.isEmpty()) inputStream = NetworkIO.decryptedInputStream(password, inputStream)
    if (compressed) inputStream = new InflaterInputStream(inputStream)
    inputStream
  }

  /** Reads from a wrapped stream. */
  private def readFromStream(is: InputStream): Network = read(wrapInputStream(is))
}

/**
 * Factory for binary or plain network readers with or without compression and encryption.
 * If password is not empty network will be encrypted.
 */
object NetworkReader {
  def apply(binary: Boolean, compressed: Boolean, password: String) = binary match {
    case true  => NetworkReaderBinary(compressed, password)
    case false => NetworkReaderPlain(compressed, password)
  }

  /**
   * Creates a network reader that can read the specified file or throw an exception if
   *  the file format is not supported. If the file is encrypted the getPassword function
   *  will be called to get the password necessary to open decrypt the file.
   */
  def apply(filepath: String, getPassword: => String) = {
    val header = Header.readFromFile(filepath)
    val compressed = header.compressed
    val password = if (header.encrypted) getPassword else ""
    header.format match {
      case Plain  => new NetworkReaderPlain(compressed, password)
      case Binary => new NetworkReaderBinary(compressed, password)
      case _      => throw new IllegalArgumentException("File format not supported: " + header.format)
    }

  }
}