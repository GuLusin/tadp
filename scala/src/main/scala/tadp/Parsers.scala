package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

object ErrorDeParseo extends RuntimeException("No se pudo parsear")
trait Parser[T] extends (String => Try[(T, String)]) {

  def <|>(parser: Parser[T]): Parser[T] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success(tuple) => Success(tuple)
      case Failure(_) => parser.apply(stringRecibido)
    }

  def <>[R](parser: Parser[R]): Parser[(T, R)] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsedElement1, s2 : String)) =>
        parser.apply(s2) match {
          case Success((parsedElement2, strToParse : String)) => Success((parsedElement1, parsedElement2), strToParse)
          case Failure(_) => Failure(ErrorDeParseo)
        }
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def ~>[R](parser: Parser[R]): Parser[R] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((_, s2 : String)) => parser.apply(s2)
      case Failure(_) => Failure(ErrorDeParseo)
    }

  def <~[R](parser: Parser[R]): Parser[T] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsedElement1, s2 : String)) =>
        parser.apply(s2) match {
          case Success((_, strToParse : String)) => Success(parsedElement1, strToParse)
          case Failure(_) => Failure(ErrorDeParseo)
        }
      case Failure(_) => Failure(ErrorDeParseo)
    }

  // FIXME:
  // en vez de Any => Any: quisiera que vaya de TipoQueDevuelveEsteParserEnSuccess => Any
  def satisfies(f : T => Boolean): Parser[T] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsed, toParse)) => if(f(parsed)) Success((parsed, toParse)) else Failure(ErrorDeParseo)
      case Failure(_) => Failure(ErrorDeParseo)
    }

  //  FIXME:
  //  val talVezIn = string("in").opt
  //  val precedencia = talVezIn <> string("fija")
  //  precedencia("infija")  --->  Success(((null,fija),))
  //  esta bien?

  def opt(): Parser[Option[T]] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsed, toParse : String)) => Success(Some(parsed), toParse)
      case Failure(_) => Success(None, stringRecibido)
    }

  // La clausura de Kleene se aplica a un parser, convirtiéndolo en otro que se puede aplicar todas las veces que sea posible o 0 veces.
  // El resultado debería ser una lista que contiene todos los valores que hayan sido parseados (podría no haber ninguno).
  def *(): Parser[T] = (stringRecibido: String) =>
    ???


  def +(): Parser[T] = (stringRecibido: String) =>
    ???

  // FIXME:
  // en vez de Any => Any: quisiera que vaya de TipoQueDevuelveEsteParserEnSuccess => Any
  def map[R](f : T => R): Parser[R] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
        // probar con map posta
      case Success((parsedElement, str : String)) => Success(f(parsedElement),str)
      case Failure(_) => Failure(ErrorDeParseo)
    }
}

object anyChar extends Parser[Char]{
  override def apply(stringRecibido: String): Try[(Char, String)] = Try(stringRecibido.head, stringRecibido.substring(1))
}

case class char(_char : Char) extends Parser[Char] {
  override def apply(stringRecibido: String): Try[(Char, String)] = if (stringRecibido.head == _char){
    Success((_char, stringRecibido.substring(1)))
  }
  else
    Failure(ErrorDeParseo)
}

object digit extends Parser[Char]{
  override def apply(stringRecibido: String): Try[(Char, String)] = if (stringRecibido.head.isDigit)
    Success(stringRecibido.charAt(0), stringRecibido.substring(1))
  else
    Failure(ErrorDeParseo)
}

case class string(s1: String) extends Parser[String] {
  override def apply(stringRecibido: String): Try[(String, String)] = if (stringRecibido.startsWith(s1))
    Success(s1, stringRecibido.substring(s1.length))
  else
    Failure(ErrorDeParseo)
}
