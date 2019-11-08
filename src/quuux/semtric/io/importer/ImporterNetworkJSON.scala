package quuux.semtric.io.importer

import scala.io.Source
import scala.util.parsing.combinator._
import java.io.InputStream
import quuux.semtric.Network
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._

/**
 * Importer for networks in JSON format.
 */
object ImporterNetworkJSON extends Importer {

  def fromStream(is: InputStream): Network = {
    val text = Source.fromInputStream(is)("UTF-8").mkString
    createNetwork()
  }

}

class JSON extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)
  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"
  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^
      { case name ~ ":" ~ value => (name, value) }
  def value: Parser[Any] = (
    obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (_.toDouble)
    | "null" ^^ (x => null)
    | "true" ^^ (x => true)
    | "false" ^^ (x => false))

  def read(json: String) = {
    parseAll(value, json) match {
      case Success(result, next) => result
      case failure               => throw new Exception(failure.toString)
    }
  }
}

object ImporterNetworkJSONExample {
  def main(args: Array[String]) {
    println("running...")
    val input = """
      {"name" : "stefan", "numbers":[{"one":1}, {"two":2}]}
      """
    val parser = new JSON
    val parsed = parser.read(input)
    println(parsed)
  }
}
