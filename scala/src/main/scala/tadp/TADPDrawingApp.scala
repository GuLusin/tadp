package tadp

import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.{Elemento, Polenta}

object TADPDrawingApp extends App {

  val elemento: Elemento = Polenta()("C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\ejemplos-dibujos\\asd.dibujitos")
  println(elemento)

  TADPDrawingAdapter forScreen { adapter =>
    elemento.agregarAdapter(adapter)
  }

}
