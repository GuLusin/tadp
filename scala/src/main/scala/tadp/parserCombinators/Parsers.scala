package tadp.parserCombinators

import scala.util.{Failure, Success, Try}

object ErrorDeParseo extends RuntimeException("No se pudo parsear")

trait Parser[+T] extends (String => Try[(T, String)]) {

  def <|>[T1 >: T](otroParser: => Parser[T1]): Parser[T1] = (stringRecibido: String) =>
    this (stringRecibido) recoverWith { case ErrorDeParseo => otroParser(stringRecibido) }


  def <>[R](parser: Parser[R]): Parser[(T, R)] = (stringRecibido: String) =>
    for {
      (parsedElement1, s2) <- this (stringRecibido)
      (parsedElement2, strToParse: String) <- parser(s2)
    } yield ((parsedElement1, parsedElement2), strToParse)


  def ~>[R](parser: Parser[R]): Parser[R] = (stringRecibido: String) =>
    this (stringRecibido) flatMap { case (_, toParse: String) => parser(toParse) }


  def <~[R](parser: Parser[R]): Parser[T] = (stringRecibido: String) =>
    for {
      (parsedElement1, s2) <- this (stringRecibido)
      (_, strToParse) <- parser(s2)
    } yield (parsedElement1, strToParse)


  def sepBy[R](separatorParser: Parser[R]): Parser[List[T]] = (stringRecibido: String) =>
    this.<>(separatorParser.~>(this).*())(stringRecibido)
      .map { case ((primerParsed, listaParsed), toParse) => (primerParsed :: listaParsed, toParse) }


  def satisfies(f: T => Boolean): Parser[T] = (stringRecibido: String) =>
    this (stringRecibido) flatMap { case (parsed, toParse) => if (f(parsed)) Success((parsed, toParse)) else Failure(ErrorDeParseo) }


  def opt(): Parser[Option[T]] = (stringRecibido: String) =>
    this (stringRecibido) match {
      case Success((parsed, toParse: String)) => Success(Some(parsed), toParse)
      case Failure(_) => Success(None, stringRecibido)
    }


  def *(): Parser[List[T]] = (stringRecibido: String) =>
    this (stringRecibido) match {
      case Success((parsed, toParse: String)) =>
        val (listaSiguienteIteracion, strSiguienteIteracion): (List[T], String) = this.*()(toParse).get
        if (listaSiguienteIteracion.isEmpty) Success(List(parsed), toParse) else Success(parsed :: listaSiguienteIteracion, strSiguienteIteracion)
      case Failure(_) => Success(List.empty[T], stringRecibido)
    }


  def +(): Parser[List[T]] = (stringRecibido: String) =>
    this.*()(stringRecibido) match {
      case Success((List(), _)) => Failure(ErrorDeParseo)
      case Success(res) => Success(res)
    }


  def map[R](f: T => R): Parser[R] = (stringRecibido: String) =>
    this (stringRecibido) map { case (parsedElement, toParse) => (f(parsedElement), toParse) }


}

object anyChar extends Parser[Char] {
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0)
      Success(stringRecibido.head, stringRecibido.substring(1))
    else
      Failure(ErrorDeParseo)
}

case class char(_char: Char) extends Parser[Char] {
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0 && stringRecibido.head == _char)
      Success((_char, stringRecibido.substring(1)))
    else
      Failure(ErrorDeParseo)
}

object digit extends Parser[Char] {
  override def apply(stringRecibido: String): Try[(Char, String)] =
    if (stringRecibido.length != 0 && stringRecibido.head.isDigit)
      Success(stringRecibido.head, stringRecibido.substring(1))
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

object integer extends Parser[Int] {
  override def apply(stringRecibido: String): Try[(Int, String)] = {
    val parserAuxiliar: Parser[(Option[Char], List[Char])] = char('-').opt() <> digit.+
    parserAuxiliar(stringRecibido) map { case ((opt: Option[Char], charList: List[Char]), str2: String) => ((opt.getOrElse("") :: charList).mkString.toInt, str2) }
  }
}


object double extends Parser[Double] {

  private def unirDouble(menos: Option[Char], pDigits: List[Char], punto: Option[Char], sDigits: Option[List[Char]]): Double = {
    ((menos.getOrElse("") :: pDigits) ++ (punto.getOrElse("") :: sDigits.getOrElse(List.empty))).mkString.toDouble
  }

  override def apply(stringRecibido: String): Try[(Double, String)] = {
    char('-').opt().<>(digit.*()).<>(char('.').opt()).<>(digit.*().opt())(stringRecibido)
      .map { case ((((menos, pDigits), punto), sDigits), toParse) => (unirDouble(menos, pDigits, punto, sDigits), toParse) }
  }

}

