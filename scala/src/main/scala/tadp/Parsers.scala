package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

object ErrorDeParseo extends RuntimeException("No se pudo parsear")
trait Parser extends (String => Try[Object]) {
  def <|>(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success(strings) => Success(strings)
      case Failure(_) => parser.apply(v1)
    }

  def <>(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success((s1, s2 : String)) =>
        parser.apply(s2) match {
          case Success((l1,_)) => Success(s1, l1)
          case Failure(_) => Failure(ErrorDeParseo)
        }
      case Failure(_) => Failure(ErrorDeParseo)
    }

  // fixme: ~> y <~ probablemente no interpreté bien que retornan
  def ~>(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success((_, s2 : String)) => parser.apply(s2)
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def <~(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success((s1, s2 : String)) =>
        if (parser.apply(s2).isSuccess)
          Success((s1, s2))
        else
          Failure(ErrorDeParseo)
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def satisfies(parser: Parser): Parser = (v1: String) =>
    ???

  def opt(parser: Parser): Parser = (v1: String) =>
    ???

  def *(parser: Parser): Parser = (v1: String) =>
    ???

  def +(parser: Parser): Parser = (v1: String) =>
    ???

}

object anyChar extends Parser{
  override def apply(v1: String): Try[(String, String)] = Try(v1.charAt(0).toString, v1.substring(1))
}

case class char(_char : Char) extends Parser {
  override def apply(v1: String): Try[(String, String)] = if (v1.startsWith(_char.toString)){
    Success(_char.toString, v1.substring(1))
  }
  else
    Failure(ErrorDeParseo)
}

object digit extends Parser{
  override def apply(v1: String): Try[(String, String)] = if (v1.charAt(0).isDigit)
    Success(v1.charAt(0).toString, v1.substring(1))
  else
    Failure(ErrorDeParseo)
}

case class string(s1: String) extends Parser {
  override def apply(v1: String): Try[(String, String)] = if (v1.startsWith(s1))
    Success(s1, v1.substring(s1.length))
  else
    Failure(ErrorDeParseo)
}
