package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

object ErrorDeParseo extends RuntimeException("No se pudo parsear")
trait Parser extends (String => Try[(String, String)]){
  def <|>(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success(strings) => Success(strings)
      case Failure(_) => parser.apply(v1)
    }

  def <>(parser: Parser): Parser = (v1: String) =>
    this.apply(v1) match {
      case Success((_, s2)) => parser.apply(s2)
      case Failure(_) => Failure(ErrorDeParseo)
    }
  /* fixme:
      val holaMundo = string("hola") <> string("mundo")
      print(holaMundo("holamundo"))

      >> Success((mundo,))

      pero debería producir un resultado exitoso con los valores "hola" y "mundo" en una tupla

   */

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
  override def apply(v1: String): Try[(String, String)] = if (v1.charAt(0).isDigit){
    Success(v1.charAt(0).toString, v1.substring(1))
  }
  else
    Failure(ErrorDeParseo)
}

case class string(string: String) extends Parser {
  override def apply(v1: String): Try[(String, String)] = if (v1.startsWith(string)){
    Success(string, v1.substring(string.length))
  }
  else
    Failure(ErrorDeParseo)
}