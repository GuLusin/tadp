package tadp.parserCombinators

import scalafx.scene.paint.Color
import tadp.internal.TADPDrawingAdapter

import scala.io.BufferedSource


trait Elemento {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter
}

trait Figura extends Elemento

case class Punto(x: Double, y: Double) {
  def getTuple: (Double, Double) = (x, y)
}

case class Triangulo(p1: Punto, p2: Punto, p3: Punto) extends Figura {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.triangle(p1.getTuple, p2.getTuple, p3.getTuple)
}

case class Rectangulo(supIzq: Punto, infDer: Punto) extends Figura {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.rectangle(supIzq.getTuple, infDer.getTuple)
}

case class Circulo(centro: Punto, radio: Double) extends Figura {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.circle(centro.getTuple, radio)
}

case class Grupo(elementos: List[Elemento]) extends Elemento {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = elementos.foldLeft(adapter)((adapter, elemento) => elemento.agregarAdapter(adapter))
}

abstract class Transformador(aplicaSobre: Elemento) extends Elemento {
  def esMismoTipoTransformacion(resto: List[Transformador]): Boolean = try resto forall this.esMismoTipoTransformacion catch {
    case _ => false
  }

  def esMismoTipoTransformacion(otraTransformacion: Transformador): Boolean =
    otraTransformacion.getClass == getClass && otraTransformacion.parametros == parametros()

  def parametros(): Any

  def seAplicaSobre(): Elemento = aplicaSobre

  def wrapTransoformacion(adapterModificado: TADPDrawingAdapter): TADPDrawingAdapter = aplicaSobre.agregarAdapter(adapterModificado).end()

  def getIgual(ahoraAplicaSobre: Elemento): Transformador

}


case class Colour(rojo: Int, verde: Int, azul: Int, _aplicaSobre: Elemento) extends Transformador(_aplicaSobre) {
  require(0 <= rojo && rojo <= 255, "Codigo de color debe estar entre 0 y 255")
  require(0 <= verde && verde <= 255, "Codigo de color debe estar entre 0 y 255")
  require(0 <= azul && azul <= 255, "Codigo de color debe estar entre 0 y 255")

  def isSameColor(colour: Colour): Boolean = rojo == colour.rojo && verde == colour.verde && azul == colour.azul

  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = wrapTransoformacion(adapter.beginColor(Color.rgb(rojo, verde, azul)))

  override def parametros(): (Int, Int, Int) = (rojo, verde, azul)

  override def getIgual(ahoraAplicaSobre: Elemento): Transformador
  = this.copy(_aplicaSobre = ahoraAplicaSobre)

}

case class Escala(factorX: Double, factorY: Double, _aplicaSobre: Elemento) extends Transformador(_aplicaSobre) {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = wrapTransoformacion(adapter.beginScale(factorX, factorY))

  override def parametros(): (Double, Double) = (factorX, factorY)

  override def getIgual(ahoraAplicaSobre: Elemento): Transformador
  = this.copy(_aplicaSobre = ahoraAplicaSobre)
}

//El ángulo debería estar entre 0 y 359 inclusive.
//Si en la descripción dada el ángulo es mayor, queremos limitarlo a esos valores usando un ángulo equivalente.
case class Rotacion(angulo: Int, _aplicaSobre: Elemento) extends Transformador(_aplicaSobre) {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = wrapTransoformacion(adapter.beginRotate(angulo))

  override def parametros(): Int = angulo

  override def getIgual(ahoraAplicaSobre: Elemento): Transformador
  = this.copy(_aplicaSobre = ahoraAplicaSobre)

}

case class Translacion(desX: Double, desY: Double, _aplicaSobre: Elemento) extends Transformador(_aplicaSobre) {
  def agregarAdapter(adapter: TADPDrawingAdapter): TADPDrawingAdapter = wrapTransoformacion(adapter.beginTranslate(desX, desY))

  override def parametros(): (Double, Double) = (desX, desY)

  override def getIgual(ahoraAplicaSobre: Elemento): Transformador = this.copy(_aplicaSobre = ahoraAplicaSobre)

}

case class Polenta() extends (String => Elemento) {

  private def getStringFromFile(fileLoc: String): String = {
    val source: BufferedSource = scala.io.Source.fromFile(fileLoc)
    val str: String = source.getLines mkString "\n"
    source.close()
    str
  }


  val coma: Parser[Char] = char(',')

  val dotPraser: Parser[Punto] = (double <~ char('@') <> double).map { case (x, y) => Punto(x, y) }

  val recParser: Parser[Rectangulo] = (string("rectangulo[") ~> dotPraser <~ coma <> dotPraser <~ char(']')) map { case (x, y) => Rectangulo(x, y) }
  val triParser: Parser[Triangulo] = (string("triangulo[") ~> dotPraser <~ coma <> dotPraser <~ coma <> dotPraser <~ char(']')) map { case ((x, y), z) => Triangulo(x, y, z) }
  val cirParser: Parser[Circulo] = (string("circulo[") ~> dotPraser <~ coma <> double <~ char(']')) map { case (centro, radio) => Circulo(centro, radio) }

  val colorParser: Parser[Colour] = (string("color[") ~> integer <~ coma <> integer <~ coma <> integer <~ string("](") <> elemParser <~ char(')'))
    .map { case (((r: Int, g: Int), b: Int), elemento: Elemento) => Colour(r, g, b, elemento) }

  val escalaParser: Parser[Escala] = (string("escala[") ~> double <~ coma <> double <~ string("](") <> elemParser <~ char(')'))
    .map { case ((x: Double, y: Double), elemento: Elemento) => Escala(x, y, elemento) }

  val rotParser: Parser[Rotacion] = (string("rotacion[") ~> integer <~ string("](") <> elemParser <~ char(')'))
    .map { case (angulo: Int, elemento: Elemento) => Rotacion(angulo % 360, elemento) }

  val translacionParser: Parser[Translacion] = (string("traslacion[") ~> double <~ coma <> double <~ string("](") <> elemParser <~ char(')'))
    .map { case ((x: Double, y: Double), elemento: Elemento) => Translacion(x, y, elemento) }

  def transformadorParser: Parser[Transformador] = colorParser <|> escalaParser <|> rotParser <|> translacionParser

  def figuraParser: Parser[Figura] = recParser <|> triParser <|> cirParser

  def elemParser: Parser[Elemento] = figuraParser <|> grupoParser <|> transformadorParser

  def grupoParser: Parser[Grupo] = (string("grupo(") ~> elemParser.sepBy(coma) <~ char(')')) map { l: List[Elemento] => Grupo(l) }


  def simplificar(elems: List[Elemento]): List[Elemento] = {
    elems match {
      case Nil => Nil

      case Rotacion(0, elemento) :: ls => simplificar(elemento :: ls)

      case Rotacion(ang1, Rotacion(ang2, elemento)) :: ls =>
        simplificar(Rotacion((ang1 + ang2) % 360, elemento) :: ls)

      case Grupo((primer: Transformador) :: (resto: List[Transformador])) :: ls
        if primer.esMismoTipoTransformacion(resto) =>
        simplificar(primer.getIgual(Grupo(primer.seAplicaSobre() :: resto.map(_.seAplicaSobre()))) :: ls)

      case Colour(_, _, _, colour: Colour) :: ls => simplificar(colour :: ls)

      case Escala(1, 1, elemento) :: ls => simplificar(elemento :: ls)

      case Escala(x1, y1, Escala(x2, y2, elemento)) :: ls =>
        simplificar(Escala(x1 * x2, y1 * y2, elemento) :: ls)

      case Translacion(0, 0, elemento) :: ls => simplificar(elemento :: ls)

      case Translacion(x1, y1, Translacion(x2, y2, elemento)) :: ls =>
        simplificar(Translacion(x1 + x2, y1 + y2, elemento) :: ls)

      case i :: is => i :: simplificar(is)
    }

  }

  def parsearString(string: String): Elemento = {
    val stringSinEspacios = string.replaceAll("\\s", "")
    val element = elemParser.+()(stringSinEspacios).get._1
    simplificar(element).head
  }


  override def apply(nombreArchivo: String): Elemento = {
    val preParsed = getStringFromFile(nombreArchivo)
    parsearString(preParsed)

  }
}