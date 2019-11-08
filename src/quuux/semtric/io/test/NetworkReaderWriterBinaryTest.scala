package quuux.semtric.io.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._
import quuux.semtric.io.reader._
import quuux.semtric.io.writer._
import quuux.semtric.io.Binary
import quuux.semtric.io.Header

class NetworkReaderWriterBinaryTest extends NetworkReaderWriterTest {

  /** Constructs readers and writers with and without encryption and compression. */
  def fixture = new {
    val filepath = "test/network.binary"

    val network = diverseNetwork

    val reader = new NetworkReaderBinary
    val writer = new NetworkWriterBinary

    val readerCompressed = new NetworkReaderBinary(true, "")
    val writerCompressed = new NetworkWriterBinary(true, "")

    val readerEncrypted = new NetworkReaderBinary(false, "password")
    val writerEncrypted = new NetworkWriterBinary(false, "password")

    val readerCompressedEncrypted = new NetworkReaderBinary(true, "password")
    val writerCompressedEncrypted = new NetworkWriterBinary(true, "password")
  }

  "ReaderWriter in Binary format" should "have matching formats" in {
    val f = fixture
    assert(f.reader.format == f.writer.format)
    assert(f.reader.format == Binary)
  }

  they should "read networks with random content written in binary format from file" in {
    val f = fixture
    val filepath = f.filepath + ".random"
    for (_ <- 0 to 10) {
      val network = randomNetwork(false) // arbitrary non-printable content is fine.
      f.writer.writeToFile(network, filepath)
      val header = Header.readFromFile(filepath)
      header should be(Header(Binary, false, false))
      assert(f.reader.readFromFile(filepath), network)
    }
  }

  they should "read networks written in default format from file" in {
    val f = fixture
    val filepath = f.filepath + ".default"
    f.writer.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Binary, false, false))
    assert(f.reader.readFromFile(filepath), f.network)
  }

  they should "read networks written in compressed format from file" in {
    val f = fixture
    val filepath = f.filepath + ".compressed"
    f.writerCompressed.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Binary, true, false))
    assert(f.readerCompressed.readFromFile(filepath), f.network)
  }

  they should "read networks written in encrypted format from file" in {
    val f = fixture
    val filepath = f.filepath + ".encrypted"
    f.writerEncrypted.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Binary, false, true))
    assert(f.readerEncrypted.readFromFile(filepath), f.network)
  }

  they should "read networks written in compressed and encrypted format from file" in {
    val f = fixture
    val filepath = f.filepath + ".compenc"
    f.writerCompressedEncrypted.writeToFile(f.network, filepath)
    val header = Header.readFromFile(filepath)
    header should be(Header(Binary, true, true))
    assert(f.readerCompressedEncrypted.readFromFile(filepath), f.network)
  }

  they should "throw Exception if password is wrong for encrypted file" in {
    val f = fixture
    val filepath = f.filepath + ".encrypted"
    f.writerEncrypted.writeToFile(f.network, filepath)
    a[Exception] should be thrownBy {
      val readerEncrypted = new NetworkReaderBinary(true, "wrong password")
      readerEncrypted.readFromFile(filepath)
    }
  }

  they should "throw UnsupportedOperationException when write or read binary format from string" in {
    val f = fixture
    a[UnsupportedOperationException] should be thrownBy {
      val string = f.writer.writeToString(f.network)
    }
    a[UnsupportedOperationException] should be thrownBy {
      val string = ""
      f.reader.readFromString(string)
    }
  }

  they should "read networks written in binary format from byte array" in {
    val f = fixture
    val array = f.writer.writeToByteArray(f.network)
    val header = Header.readFromByteArray(array)
    header should be(Header(Binary, false, false))
    assert(f.reader.readFromByteArray(array), f.network)
  }

  they should "throw Exception if input is in wrong format" in {
    val f = fixture
    a[Exception] should be thrownBy {
      f.reader.readFromFile("test/no_network.txt")
    }
  }

}