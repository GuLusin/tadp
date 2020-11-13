package tadp

import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.{Elemento, Polenta}

object DrawingFromFile extends App {

  val elemento : Elemento = Polenta()("C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\ejemplos-dibujos\\composicionC")
  println("Elemento luego de realizar parseo y simplificacion:")
  println(elemento)

  TADPDrawingAdapter
    .forScreen { adapter =>
      elemento.agregarAdapter(adapter)
    }

}
