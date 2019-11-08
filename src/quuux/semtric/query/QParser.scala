package quuux.semtric.query

import scala.util.parsing.combinator.JavaTokenParsers
import quuux.semtric._
import quuux.semtric.query._

private class QParser extends JavaTokenParsers {
  val muliLineComment = """(?s)/#.+?#/""" // (?s) enables multi-line matching.
  val singleLineComment = """#.+?$"""     // single-line comment.

  def expr = or ~ rep("|" ~> or) ^^ { case t ~ l => l.foldLeft(t)(QOr(_, _)) }
  def or = alt ~ rep(";" ~> alt) ^^ { case t ~ l => l.foldLeft(t)(QAlt(_, _)) }
  def alt = and ~ rep("&" ~> and) ^^ { case t ~ l => l.foldLeft(t)(QAnd(_, _)) }
  def and = comp ~ rep("-" ~> comp) ^^ { case t ~ l => l.foldLeft(t)(QComplement(_, _)) }
  def comp = rel ~ rep(rel) ^^ { case t ~ l => l.foldLeft(t)(QRelation(_, _)) }
  def rel: Parser[QNode] = 
    function | "(" ~> expr <~ ")" | variableStar | variable | value | regex

  def value: Parser[QValue[_]] = valueString | valueInt | valueDouble
  def valueString: Parser[QValue[String]] = string ^^ { s => QValue(s) }
  def valueInt: Parser[QValue[Int]] = wholeNumber ^^ { n => QValue(n.toInt) }
  def valueDouble: Parser[QValue[Double]] = decimalNumber ^^ { n => QValue(n.toDouble) }
  
  def regex: Parser[QRegEx] = "/" ~> """[^/]+""".r <~ "/" ^^ { QRegEx(_) }

  def variable: Parser[QVariable] = "<" ~> varname <~ ">" ^^ { QVariable(_) }
  def variableStar: Parser[QNode] =
    "<" ~> value <~ "*>" ^^ { v => QRelation(v, QVariable(v.value.toString)) }
  def function: Parser[QNode] =
    ("?" ~> ident <~ "(") ~ repsep(expr, ",") <~ ")" ^^ { case n ~ a => QFunction(n, a) }

  def varname = """[^\*<>]*""".r
  def string = quoted | unquoted
  def unquoted = ident
  def quoted = """'([^']|'')*'""".r ^^ { _.drop(1).dropRight(1).replace("''", "'") }

  def read(query: String) = {
    val cleanQuery = query.replaceAll(muliLineComment, "").replaceAll(singleLineComment, "")
    parseAll(expr, cleanQuery) match {
      case Success(result, next) => result
      case failure               => throw new Exception(failure.toString)
    }
  }
}
