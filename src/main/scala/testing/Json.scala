package testing

import atto.parser.character.char
import atto.parser.text.{string, stringLiteral}
import atto.parser.combinator.endOfInput
import atto.parser.numeric.double
import atto.syntax.parser._
import atto.Parser

object Json {
  sealed trait Json
  case object JNull extends Json
  final case class JBoolean(value: Boolean) extends Json
  final case class JNumber(value: Double) extends Json
  final case class JString(value: String) extends Json
  final case class JArray(value: Vector[Json]) extends Json
  final case class JObject(value: Map[String, Json]) extends Json

  object Parser {
    val jNull: Parser[JNull.type] =
      string("null") >| JNull
    val jBoolean: Parser[JBoolean] =
      (string("true") >| true | string("false") >| false) -| JBoolean
    val jNumber: Parser[JNumber] =
      double -| JNumber
    val jString: Parser[JString] =
      stringLiteral -| JString
    lazy val jArray: Parser[JArray] =
      (char('[') ~> json.sepBy1(char(',')) <~ char(']')) -| { l =>
        JArray(l.toList.toVector)
      }
    lazy val jObject: Parser[JObject] =
      (char('{') ~> ((stringLiteral <~ char(':')) ~ json).sepBy1(char(',')) <~ char('}')) -| { l =>
        JObject(l.toList.toMap)
      }
    lazy val json: Parser[Json] =
      jNull | jBoolean | jNumber | jString | jArray | jObject
    lazy val jsonOnly: Parser[Json] =
      json <~ endOfInput
  }

  def parse(s: String): Option[Json] =
    Parser.jsonOnly.parseOnly(s).option

  def printString(s: String): String = s""""${strToUni(s)}""""

  def print(json: Json): String = json match {
    case JNull => "null"
    case JBoolean(a) => if (a) "true" else "false"
    case JNumber(a) => a.toString
    case JString(a) => printString(a)
    case JArray(a) => s"[${a.map(print).mkString(",")}]"
    case JObject(a) =>
      val entries = a.map { case (k, v) => s"${printString(k)}:${print(v)}" }
      val body = entries.mkString(",")
      s"{$body}"
  }

  private def charToUni(c: Char) = {
    if (c > 127 && !c.isControl) {
      c.toString
    } else {
      val hex = Integer.toHexString(c).toUpperCase.reverse.padTo(4, '0').reverse
      s"\\u$hex"
    }
  }

  private def strToUni(s: String) = s.map(charToUni).mkString("")
}
