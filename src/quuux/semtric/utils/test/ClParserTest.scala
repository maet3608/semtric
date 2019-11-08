package quuux.semtric.utils.test

import org.scalatest._
import quuux.semtric.utils._

class ClParserTest extends FlatSpec with Matchers {

  def fixture = new {
    var textCmdExecuted = false

    object ClTest extends ClCommand("test", "test command",
      ClParameter("required", "Name of a required parameter", true),
      ClParameter("optional", "Name of an optional parameter", default = "opt"),
      ClFlag("flag", "Name of boolean flag", "false")) {
      def execute() { textCmdExecuted = true }
    }

    val clTest = ClTest
    val parser = new ClParser(clTest)
  }

  "ClCommand" should "have getter for parameters" in {
    val f = fixture
    val args = Array("test", "req_val", "opt_val")
    f.parser.execute(args)
    f.clTest.parameter("required").str should be("req_val")
    f.clTest.parameter("optional").str should be("opt_val")
  }

  it should "have getter for flags" in {
    val f = fixture
    val args = Array("test", "req_val", "--flag=true")
    f.parser.execute(args)
    f.clTest.flag("flag").bool should be(true)
    f.clTest.flag("flag").str should be("true")
  }

  "ClParser" should "execute command" in {
    val f = fixture
    val args = Array("test", "req_val")
    f.parser.execute(args)
    f.textCmdExecuted should be(true)
  }

  it should "set parameter" in {
    val f = fixture
    val args = Array("test", "req_val")
    f.parser.execute(args)
    f.ClTest.parameter("required").str should be("req_val")
  }

  it should "set default values for parameter" in {
    val f = fixture
    val args = Array("test", "req_val")
    f.parser.execute(args)
    f.ClTest.parameter("optional").str should be("opt")
  }

  it should "set flag with value" in {
    val f = fixture
    val args = Array("test", "req_val", "--flag=true")
    f.parser.execute(args)
    f.ClTest.flag("flag").str should be("true")
  }

  it should "set flag without" in {
    val f = fixture
    val args = Array("test", "req_val", "--flag")
    f.parser.execute(args)
    f.ClTest.flag("flag").str should be("true")
  }

  it should "allow flag names with single hyphen" in {
    val f = fixture
    val args = Array("test", "req_val", "-flag=true")
    f.parser.execute(args)
    f.ClTest.flag("flag").str should be("true")
  }

  it should "allow flag names without hyphen" in {
    val f = fixture
    val args = Array("test", "cmd", "flag=true")
    f.parser.execute(args)
    f.ClTest.flag("flag").str should be("true")
  }

  it should "allow flag before parameters" in {
    val f = fixture
    val args = Array("test", "--flag=true", "req_val")
    f.parser.execute(args)
    f.ClTest.parameter("required").str should be("req_val")
    f.ClTest.flag("flag").str should be("true")
  }

  it should "throw exception if required parameter is missing" in {
    val f = fixture
    val args = Array("test")
    a[IllegalArgumentException] should be thrownBy {
      f.parser.execute(args)
    }
  }

  it should "throw exception if parameter or flag is unknown" in {
    val f = fixture
    val args = Array("test", "-unknown")
    a[IllegalArgumentException] should be thrownBy {
      f.parser.execute(args)
    }
  }

  it should "throw exception if flag is unknown" in {
    val f = fixture
    val args = Array("test", "--unknownflag=true", "req_val")
    a[IllegalArgumentException] should be thrownBy {
      f.parser.execute(args)
    }
  }

}