package quuux.semtric.io.test

import org.scalatest._
import quuux.semtric.io._
import java.io.FileOutputStream

class HeaderTest extends FlatSpec with Matchers {

  "Header" should "be comparable" in {
    val header1 = Header(Plain, false, false)
    val header2 = Header(Plain, false, false)
    val header3 = Header(Plain, true, false)
    assert(header1 == header2)
    assert(header1 != header3)
  }  
  
  it should "read the same as written" in {
    val path = "test/header.bin"
    val fos = new FileOutputStream(path)
    Header.write(fos, Binary, true, false)
    val header = Header.readFromFile(path)
    header should be (Header(Binary, true, false))
    fos.close()
  }
}