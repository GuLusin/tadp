package tadp

import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.{Elemento, Polenta}

object DrawingFromFile extends App {

  val elemento: Elemento = Polenta()("C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\ejemplos-dibujos\\carpinchoDeBoca")

  TADPDrawingAdapter forScreen {
    elemento.agregarAdapter
  }

}
