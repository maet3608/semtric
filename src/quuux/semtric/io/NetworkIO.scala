package quuux.semtric.io

import javax.crypto.Cipher
import javax.crypto.spec.DESKeySpec
import javax.crypto.SecretKeyFactory
import javax.crypto.SecretKey
import javax.crypto.CipherInputStream
import java.io.InputStream
import java.io.OutputStream
import javax.crypto.CipherOutputStream
import javax.crypto.spec.IvParameterSpec
import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import java.security.SecureRandom
import java.io.IOException
import quuux.semtric.utils.ProgressTracker

/** 
 *  Base class for Network readers and writers.
 */
abstract class NetworkIO {
  /** Keeps track of IO progress. */
  protected var progress = ProgressTracker()

  /** Sets a new progress tracker. Note that there is a default racker. */
  def setProgressTracker(tracker:ProgressTracker) { progress = tracker }
  
  /**
   * getProgress is a function that is called during reading (e.g. to update a progress bar)
   *  its double parameter takes values in [0,1]. If set to null no progress is reported.
   */
  def setProgressListener(getProgress: Double => Unit) { progress.setListener(getProgress) }
  
  /** Format of the reader or writer. Must match between corresponding readers and writers. */
  def format: Format  
}


/**
 * Contains static utility methods for network readers and writers.
 */
object NetworkIO {
  private val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
  private val digest = MessageDigest.getInstance("SHA-256")

  /** Replaces newline by special character to write multi-line strings. */
  def encodeNewline(value: Any) = value match {
    //case str: String => str.replace(NEWLINE, NL_CODE); str.re
    case str: String => str.replaceAll("(\r\n)|\n|\r", "\b")
    case _           => value.toString
  }

  /** Decodes special character as newline when reading strings. */
  def decodeNewline(value: String) = value.replace('\b', '\n')

  /** Returns encryption specification for a given password. */
  private def createKeySpec(password: String): SecretKeySpec = {
    // see: http://stackoverflow.com/questions/5520640/encrypting-and-decrypting-using-java
    digest.update(password.getBytes)
    new SecretKeySpec(digest.digest.take(16), "AES")
  }

  /** Returns an decrypted input stream for the given password that wraps the given input stream. */
  def decryptedInputStream(password: String, is: InputStream): CipherInputStream = {
    val keySpec = createKeySpec(password)
    val iv = Array.ofDim[Byte](cipher.getBlockSize)
    is.read(iv)
    cipher.init(Cipher.DECRYPT_MODE, keySpec, new IvParameterSpec(iv))
    new CipherInputStream(is, cipher)
  }

  /** Returns an encrypted output stream for the given password that wraps the given output stream. */
  def encryptedOutputStream(password: String, os: OutputStream): CipherOutputStream = {
    val keySpec = createKeySpec(password)
    val iv = Array.ofDim[Byte](cipher.getBlockSize)
    (new SecureRandom()).nextBytes(iv)
    os.write(iv)
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, new IvParameterSpec(iv))
    new CipherOutputStream(os, cipher)
  }

}