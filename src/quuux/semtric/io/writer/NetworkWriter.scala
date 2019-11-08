package quuux.semtric.io.writer

import java.io.OutputStream
import java.io.FileOutputStream
import java.io.ByteArrayOutputStream
import java.io.File
import quuux.semtric.Network
import java.util.zip.DeflaterOutputStream
import quuux.semtric.io.NetworkIO
import quuux.semtric.io.Format
import quuux.semtric.io.Header

/**
 * Base class for network writers  that requires the implementation of write in
 * derived classes and provides writing from files, strings and byte arrays.
 * Networks can be written in a compressed format and will be encrypted if a non-empty password
 * is given. 
 * see NetworkIOBenchmark for a speed comparision of different readers and writers.
 */
abstract class NetworkWriter(compressed:Boolean, password:String) extends NetworkIO {  
  
  /** Writes the network to output stream. Must be implemented by derived classes. */
  def write(network: Network, os: OutputStream): Unit

  /** Writes the network to file. */
  def writeToFile(network: Network, filepath: String) {
    writeToStream(network, new FileOutputStream(filepath))
  }

  /** Writes the network to file. */
  def writeToFile(network: Network, file: File) {
    writeToStream(network, new FileOutputStream(file))
  }
  
  /** Writes the network to the console. */
  def writeToConsole(network: Network) {
    writeToStream(network, System.out)
  }  

  /** Writes the network to a string. */
  def writeToString(network: Network): String = {
    val baos = new ByteArrayOutputStream()
    writeToStream(network, baos)
    baos.close()
    baos.toString("UTF-8")
  }

  /** Writes the network to a byte array. */
  def writeToByteArray(network: Network): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    writeToStream(network, baos)
    baos.close()
    baos.toByteArray()
  }
  

  
  /** Writes to the output stream; possibly with compression and/or encryption. */
  private def writeToStream(network: Network, os: OutputStream) {
    var outputStream = os
    Header.write(os, format, compressed, !password.isEmpty())
    if (!password.isEmpty()) outputStream = NetworkIO.encryptedOutputStream(password, outputStream)
    if (compressed) outputStream = new DeflaterOutputStream(outputStream)
    write(network, outputStream)
  }
}

/**
 * Factory for binary or plain network writers with or without compression and encryption.
 * If password is not empty network will be encrypted.
 */
object NetworkWriter {
  def apply(binary:Boolean, compressed: Boolean, password: String) = binary match {
    case true => NetworkWriterBinary(compressed, password) 
    case false => NetworkWriterPlain(compressed, password)
  } 
}