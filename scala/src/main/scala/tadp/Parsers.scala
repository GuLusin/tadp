package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

object ErrorDeParseo extends RuntimeException("No se pudo parsear")
trait Parser extends (String => Try[(Any, String)]) {
  def <|>(parser: Parser): Parser = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success(tuple) => Success(tuple)
      case Failure(_) => parser.apply(stringRecibido)
    }

  def <>(parser: Parser): Parser = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsedElement1, s2 : String)) =>
        parser.apply(s2) match {
          case Success((parsedElement2, strToParse : String)) => Success((parsedElement1, parsedElement2), strToParse)
          case Failure(_) => Failure(ErrorDeParseo)
        }
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def ~>(parser: Parser): Parser = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((_, s2 : String)) => parser.apply(s2)
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def <~(parser: Parser): Parser = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsedElement1, s2 : String)) =>
        parser.apply(s2) match {
          case Success((_, strToParse : String)) => Success(parsedElement1, strToParse)
          case Failure(_) => Failure(ErrorDeParseo)
        }
      case Failure(_) => Failure(ErrorDeParseo)
    }


//  def satisfies(condicion: ???): Parser = (stringRecibido: String) =>
//    ???

  //  fixme:
  //  val talVezIn = string("in").opt
  //  val precedencia = talVezIn <> string("fija")
  //  precedencia("infija")  --->  Success(((null,fija),))
  //  esta bien?

  def opt(): Parser = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success(res) => Success(res)
      case Failure(_) => Success((null, stringRecibido))
    }

  def *(): Parser = (stringRecibido: String) =>
    ???

  def +(): Parser = (stringRecibido: String) =>
    ???

}

object anyChar extends Parser{
  override def apply(stringRecibido: String): Try[(Char, String)] = Try(stringRecibido.charAt(0), stringRecibido.substring(1))
}

case class char(_char : Char) extends Parser {
  override def apply(stringRecibido: String): Try[(Char, String)] = if (stringRecibido.startsWith(_char.toString)){
    Success((_char, stringRecibido.substring(1)))
  }
  else
    Failure(ErrorDeParseo)
}

object digit extends Parser{
  override def apply(stringRecibido: String): Try[(Char, String)] = if (stringRecibido.charAt(0).isDigit)
    Success(stringRecibido.charAt(0), stringRecibido.substring(1))
  else
    Failure(ErrorDeParseo)
}

case class string(s1: String) extends Parser {
  override def apply(stringRecibido: String): Try[(String, String)] = if (stringRecibido.startsWith(s1))
    Success(s1, stringRecibido.substring(s1.length))
  else
    Failure(ErrorDeParseo)
}
