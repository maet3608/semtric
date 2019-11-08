package quuux.semtric.io.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.io.writer._
import quuux.semtric.io.reader._
import quuux.semtric.io._


class NetworkReaderTest extends NetworkReaderWriterTest {
  
  "NetworkReader" should "create correct readers for given file" in {
    val network = diverseNetwork

    val pathBase = "test/network"
    val params = List(("default", false, ""), ("comp", true, ""), ("enc", false, "pwd"),
      ("comp-enc", true, "pwd"))
      
    // Write files in all formats.  
    for ((mode, compressed, password) <- params) {
      val plnPath = pathBase + ".plain." + mode
      val binPath = pathBase + ".binary." + mode
      val writerPln = new NetworkWriterPlain(compressed, password)
      val writerBin = new NetworkWriterBinary(compressed, password)
      writerPln.writeToFile(network, plnPath)
      writerBin.writeToFile(network, binPath)
    }
    
    // Create readers for files in all formats.  
    for ((mode, compressed, password) <- params) {
      val plnPath = pathBase + ".plain." + mode
      val binPath = pathBase + ".binary." + mode
      val readerPln = NetworkReader(plnPath, "pwd")
      val readerBin = NetworkReader(binPath, "pwd")
      
      readerPln.format should be (Plain)
      readerPln.compressed should be (compressed)
      readerPln.password.isEmpty() should be (password.isEmpty())
      readerBin.format should be (Binary)
      readerBin.compressed should be (compressed)
      readerBin.password.isEmpty() should be (password.isEmpty())      
    }
    
  }
  
}