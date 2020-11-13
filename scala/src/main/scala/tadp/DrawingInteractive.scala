package tadp

import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.Polenta

object DrawingInteractive extends App {

  TADPDrawingAdapter.forInteractiveScreen { Polenta().parsearString(_).agregarAdapter(_) }

//  TADPDrawingAdapter.forInteractiveScreen { (imageDescription, adapter) =>
//    Polenta().parsearString(imageDescription).agregarAdapter(adapter)
//  }

}
