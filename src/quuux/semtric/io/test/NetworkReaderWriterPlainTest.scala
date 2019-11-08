package quuux.semtric.io.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import quuux.semtric.io.reader._
import quuux.semtric.io.writer._
import quuux.semtric.io.Plain
import quuux.semtric.io.Header

class NetworkReaderWriterPlainTest extends NetworkReaderWriterTest {

  /** Constructs readers and writers with and without encryption and compression. */
  def fixture = new {
    val filepath = "test/network.plain"

    val network = diverseNetwork

    val reader = new NetworkReaderPlain
    val writer = new NetworkWriterPlain

    val readerCompressed = new NetworkReaderPlain(true, "")
    val writerCompressed = new NetworkWriterPlain(true, "")

    val readerEncrypted = new NetworkReaderPlain(false, "password")
    val writerEncrypted = new NetworkWriterPlain(false, "password")
    
    val readerCompressedEncrypted = new NetworkReaderPlain(true, "password")
    val writerCompressedEncrypted = new NetworkWriterPlain(true, "password")
  }

  "ReaderWriter in plain format" should "have matching formats" in {
    val f = fixture
    assert(f.reader.format == f.writer.format)
    assert(f.reader.format == Plain)
  }

  they should "read networks with random but printable content from file" in {
    val f = fixture
    val filepath = f.filepath + ".random"
    for (_ <- 0 to 10) {
      val network = randomNetwork(true) // needs to be printable. 
      f.writer.writeToFile(network, filepath)
      val header = Header.readFromFile(filepath)
      header should be(Header(Plain, false, false))
      assert(f.reader.readFromFile(filepath), network)
    }
  }

  they should "read networks in default format from file" in {
    val f = fixture
    val filepath = f.filepath + ".default"
    f.writer.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Plain, false, false))
    assert(f.reader.readFromFile(filepath), f.network)
  }

  they should "read networks written in compressed format from file" in {
    val f = fixture
    val filepath = f.filepath + ".compressed"
    f.writerCompressed.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Plain, true, false))
    assert(f.readerCompressed.readFromFile(filepath), f.network)
  }

  they should "read networks written in encrypted format from file" in {
    val f = fixture
    val filepath = f.filepath + ".encrypted"
    f.writerEncrypted.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Plain, false, true))
    assert(f.readerEncrypted.readFromFile(filepath), f.network)
  }
  
  they should "read networks written in compressed and encrypted format from file" in {
    val f = fixture
    val filepath = f.filepath + ".compenc"
    f.writerCompressedEncrypted.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Plain, true, true))
    assert(f.readerCompressedEncrypted.readFromFile(filepath), f.network)
  }  

  they should "throw IllegalArgumentException if password is wrong for encrypted file" in {
    val f = fixture
    val filepath = f.filepath + ".encrypted"
    f.writerEncrypted.writeToFile(f.network, filepath)
    a[IllegalArgumentException] should be thrownBy {
      val readerEncrypted = new NetworkReaderBinary(true, "wrong password")
      readerEncrypted.readFromFile(filepath)
    }
  }

  they should "read networks written in default format from string" in {
    val f = fixture
    val string = f.writer.writeToString(f.network)
    val header = Header.readFromString(string)
    header should be(Header(Plain, false, false))
    assert(f.reader.readFromString(string), f.network)
  }

  they should "read networks written in default format from byte array" in {
    val f = fixture
    val array = f.writer.writeToByteArray(f.network)
    val header = Header.readFromByteArray(array)
    header should be(Header(Plain, false, false))
    assert(f.reader.readFromByteArray(array), f.network)
  }
  
  they should "read networks written in compressed encrypted default format from byte array" in {
    val f = fixture
    val array = f.writerCompressedEncrypted.writeToByteArray(f.network)
    val header = Header.readFromByteArray(array)
    header should be(Header(Plain, true, true))
    assert(f.readerCompressedEncrypted.readFromByteArray(array), f.network)
  }  
  
  they should "throw UnsupportedOperationException when write or read compressed or enccrypted from string" in {
    val f = fixture
    a[UnsupportedOperationException] should be thrownBy {
      val string = f.writerCompressed.writeToString(f.network)
    }
    a[UnsupportedOperationException] should be thrownBy {
      val string = ""
      f.readerCompressed.readFromString(string)
    }
    a[UnsupportedOperationException] should be thrownBy {
      val string = f.writerEncrypted.writeToString(f.network)
    }
    a[UnsupportedOperationException] should be thrownBy {
      val string = ""
      f.readerEncrypted.readFromString(string)
    }    
  }    
      
  they should "throw IllegalArgumentException if input is in wrong format" in {
    val f = fixture
    a[IllegalArgumentException] should be thrownBy {
      f.reader.readFromFile("test/no_network.txt")
    }
  }

}