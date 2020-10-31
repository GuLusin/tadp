package tadp

import scalafx.scene.paint.Color
import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.{Parser, char, double, integer, string}

object TADPDrawingApp extends App {



  def parametros[T](parser :Parser[T]) : Parser[List[T]] = (stringRecibido :String) => {
    (( char('[') ~> parser.sepBy(string(", ")) ) <~ char(']'))(stringRecibido)
  }

  def funcion[T](nombre :String, tipoParametro :Parser[T]) : Parser[List[T]] = (stringRecibido :String) => {
    (string(nombre) ~> parametros(tipoParametro))(stringRecibido)
  }


  val escala = (string("escala") <> char('[')) ~> double <> ((string(", ") ~> double) <~ char(']'))

  val escala2 = parametros(integer)

  val punto = (integer <~ string(" @ ")) <> integer

  val parDePuntos = parametros(punto)

  val color = funcion("color",integer)

  val grupo = funcion("grupo",color)

  println(escala2("[45, 45, 415, 1000]"))

  println( parDePuntos("[0 @ 0, 400 @ 400]"))



  println(escala("escala[1.45, 1.45]"))

  println(punto("45 @ 45"))



  // Ejemplo de uso del drawing adapter:
//  TADPDrawingAdapter
//    .forScreen { adapter =>
//      adapter
//        .beginColor(Color.rgb(100, 100, 100))
//        .rectangle((200, 200), (400, 400))
//        .end()
//    }
}

