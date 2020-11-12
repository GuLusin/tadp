package tadp.parserCombinators

import scala.util.{Failure, Success}

trait Elemento

case class Punto(x : Double, y : Double) extends Elemento

case class Triangulo(p1 : Punto, p2 : Punto, p3 : Punto) extends Elemento

case class Rectangulo(supIzq : Punto, infDer : Punto) extends Elemento

case class Circulo(centro : Punto, radio : Double) extends Elemento

case class Grupo(elementos : List[Elemento]) extends Elemento

//case aca rompe
class Transformador(aplicaSobre : Elemento) extends Elemento

case class color(rojo : Int, verde : Int, azul : Int, _aplicaSobre : Elemento) extends Transformador(_aplicaSobre){
  require( 0 <= rojo  && rojo  <= 255 , "Codigo de color debe estar entre 0 y 255" )
  require( 0 <= verde && verde <= 255 , "Codigo de color debe estar entre 0 y 255" )
  require( 0 <= azul  && azul  <= 255 , "Codigo de color debe estar entre 0 y 255" )
}

case class Escala(factorX : Double, factorY : Double, _aplicaSobre : Elemento) extends Transformador(_aplicaSobre)

//El ángulo debería estar entre 0 y 359 inclusive.
//Si en la descripción dada el ángulo es mayor, queremos limitarlo a esos valores usando un ángulo equivalente.
case class Rotacion(angulo : Int, _aplicaSobre : Elemento) extends Transformador(_aplicaSobre)

case class Translacion(desX : Double, desY : Double, _aplicaSobre : Elemento) extends Transformador(_aplicaSobre)

object polenta extends App{
  val str : String = "rectangulo[186 @ 0, 400 @ 150]"


  val dotPraser : Parser[Punto] = (double <~ string(" @ ") <> double).map {case (x, y) => Punto(x, y)}
  val recParser : Parser[Rectangulo] = (string("rectangulo[") ~> dotPraser <~ string(", ") <> dotPraser <~ char(']')) map { case (x, y) => Rectangulo(x, y) }
  val triParser : Parser[Triangulo] = (string("triangulo[") ~> dotPraser <~ string(", ") <> dotPraser <~ string(", ") <> dotPraser <~ char(']')) map { case ((x, y), z) => Triangulo(x, y, z) }
  val cirParser : Parser[Circulo] = (string("Circulo[") ~> dotPraser <~ string(", ") <> double <~ char(']')) map { case (centro, radio) => Circulo(centro, radio) }


  val parsersPosibles : List[Parser[ _ <: Elemento ]] = List(recParser, triParser, cirParser)

  def sentences(unStr : String): List[Elemento] = {
    val unParser = parsersPosibles.find(_.apply(unStr).isSuccess)

    unParser.get.apply(unStr) match {
      case Success((parsed, toParse)) if toParse == "" => List(parsed)
      case Success((parsed, toParse)) => parsed :: sentences(toParse)
      case Failure(_) => List()
    }


  }

  println(sentences(str))







}