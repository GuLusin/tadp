package tadp.parserCombinators
import scala.annotation.tailrec
import scala.util.Try

trait Elemento

case class Punto(x : Double, y : Double) extends Elemento


trait Figura extends Elemento

case class Triangulo(p1 : Punto, p2 : Punto, p3 : Punto) extends Figura

case class Rectangulo(supIzq : Punto, infDer : Punto) extends Figura

case class Circulo(centro : Punto, radio : Double) extends Figura


case class Grupo(elementos : List[Elemento]) extends Elemento

case class TokenFinGrupo() extends Elemento



//case aca rompe
class Transformador(aplicaSobre : Elemento) extends Elemento

case class Color(rojo : Int, verde : Int, azul : Int, _aplicaSobre : Elemento) extends Transformador(_aplicaSobre){
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

  val source = scala.io.Source.fromFile("C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\parserCombinators\\asd.dibujitos")
  var str = source.getLines mkString "\n"

  source.close()


  str = str.replaceAll("\\s","")

//  println(str)

  val coma : Parser[Char] = char(',')

  val dotPraser : Parser[Punto] = (double <~ char('@') <> double).map { case (x, y) => Punto(x, y) }

  val recParser : Parser[Rectangulo] = (string("rectangulo[") ~> dotPraser <~ coma <> dotPraser <~ char(']')) map { case (x, y) => Rectangulo(x, y) }
  val triParser : Parser[Triangulo] = (string("triangulo[") ~> dotPraser <~ coma <> dotPraser <~ coma <> dotPraser <~ char(']')) map { case ((x, y), z) => Triangulo(x, y, z) }
  val cirParser : Parser[Circulo] = (string("circulo[") ~> dotPraser <~ coma <> double <~ char(']')) map { case (centro, radio) => Circulo(centro, radio) }

  val colorParser : Parser[Color] = (string("color[") ~> integer <~ coma <> integer <~ coma <> integer <~ string("](") <> elemParser <~ char(')'))
    .map {case (((r : Int, g : Int), b: Int), elemento: Elemento) => Color(r, g, b, elemento) }

  val escalaParser : Parser[Escala] = (string("escala[") ~> double <~ coma <> double <~ string("](") <> elemParser <~ char(')'))
    .map {case ((x: Double, y: Double), elemento: Elemento) => Escala(x, y, elemento) }

  val rotParser : Parser[Rotacion] = (string("rotacion[") ~> integer <~ string("](") <> elemParser <~ char(')'))
    .map {case (angulo : Int, elemento: Elemento) => Rotacion(angulo % 360, elemento) }

  val translacionParser : Parser[Translacion] = (string("translacion[") ~> double <~ coma <> double <~ string("](") <> elemParser <~ char(')'))
    .map {case ((x: Double, y: Double), elemento: Elemento) => Translacion(x, y, elemento) }

  def transformadorParser : Parser[Transformador] = colorParser <|> escalaParser <|> rotParser <|> translacionParser

  def figuraParser : Parser[Figura] = recParser <|> triParser <|> cirParser

  def elemParser : Parser[Elemento] = figuraParser <|> grupoParser <|> transformadorParser

  def grupoParser : Parser[Grupo] = (string("grupo(") ~> elemParser.sepBy(coma) <~ char(')')) map { l: List[Elemento] => Grupo(l) }


  val aux = elemParser(str)

  println(aux)

  if(aux.isSuccess){
    println("Sin simplificar:")
    println(aux.get._1)
  }else{
    println("rompio todo, pa")
    println(aux)
  }

  @tailrec
  def simplificador(elemento: Elemento) : Elemento = {
    elemento match {
      case figura: Figura => figura
      case Grupo(elemento :: Nil) => simplificador(elemento)
//      case Grupo(elementos : List[Elemento]) => Grupo(simplificador(elementos))
      case a => a
    }
  }


  if(aux.isSuccess){
    println("Simplificado:")
    println(simplificador(aux.get._1))
  }else{
    println("rompio todo, pa")
    println(aux)

  }


}