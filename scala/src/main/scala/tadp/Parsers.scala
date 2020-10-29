package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

// TODO: 
//  case Failure(_) => parser.apply(stringRecibido) está por todooos lados

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


  def satisfies(f : T => Boolean): Parser[T] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsed, toParse)) => if(f(parsed)) Success((parsed, toParse)) else Failure(ErrorDeParseo)
      case Failure(_) => Failure(ErrorDeParseo)
    }

//  FIXME:
//   esta bien option[T]?
  def opt(): Parser[Option[T]] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsed, toParse : String)) => Success(Some(parsed), toParse)
      case Failure(_) => Success(None, stringRecibido)
    }

//  La clausura de Kleene se aplica a un parser, convirtiéndolo en otro que se puede aplicar todas las veces que sea posible o 0 veces.
//  El resultado debería ser una lista que contiene todos los valores que hayan sido parseados (podría no haber ninguno).
//  FIXME: anyChar.*() devuelve una lista de Char, estaria bueno que devuelva un String
  def *(): Parser[List[T]] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsed, toParse : String)) =>
        val (listaSiguienteIteracion, strSiguienteIteracion) : (List[T], String) = this.*().apply(toParse).get
        if (listaSiguienteIteracion.isEmpty) Success(List(parsed), toParse) else Success(parsed :: listaSiguienteIteracion , strSiguienteIteracion)
      case Failure(_) => Success(List.empty[T], stringRecibido)
    }


//  Es como la clausura de Kleene pero requiere que el parser se aplique al menos UNA vez.
  def +(): Parser[List[T]] = (stringRecibido: String) =>
      this.*().apply(stringRecibido) match {
        case Success((List(),_)) => Failure(ErrorDeParseo)
        case Success(res) => Success(res)
      }

// TODO: probablemente se pueda usar directamente la funcion map de Try
//  y hacer algo del estilo:     this.apply(stringRecibido).map(f)
  def map[R](f : T => R): Parser[R] = (stringRecibido: String) =>
    this.apply(stringRecibido) match {
      case Success((parsedElement, str : String)) => Success(f(parsedElement),str)
      case Failure(_) => Failure(ErrorDeParseo)
    }
}

//  TODO:
//    En muchos lados hacemos:
//    if condicion(strAEvaluar):
//     Success(tuplaDeRetorno)
//    else
//     Failure(ErrorDeParseo)
//
//    podriamos usar una funcion privada tipo evaluar(condicion, tuplaDeRetorno)
//    y evitar repeticion de codigo

object anyChar extends Parser[Char]{
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0)
      Success(stringRecibido.head, stringRecibido.substring(1))
    else
      Failure(ErrorDeParseo)
}

case class char(_char : Char) extends Parser[Char] {
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0 && stringRecibido.head == _char) {
      // FIXME: no usar try, devolver Failure o Success
      Try((_char, stringRecibido.substring(1)))
    } else
      Failure(ErrorDeParseo)
}

object digit extends Parser[Char]{
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0 && stringRecibido.head.isDigit)
      Success(stringRecibido.charAt(0), stringRecibido.substring(1))
    else
      Failure(ErrorDeParseo)
}

case class string(s1: String) extends Parser[String] {
  override def apply(stringRecibido: String): Try[(String, String)] =
    if (stringRecibido.length != 0 && stringRecibido.startsWith(s1))
      Success(s1, stringRecibido.substring(s1.length))
    else
      Failure(ErrorDeParseo)
}
