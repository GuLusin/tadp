package tadp

import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.Polenta

object DrawingInteractive extends App {

  TADPDrawingAdapter.forInteractiveScreen { Polenta().parsearString(_).agregarAdapter(_) }


}
