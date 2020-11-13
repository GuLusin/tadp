package tadp

import scalafx.scene.paint.Color
import tadp.internal.TADPDrawingAdapter
import tadp.parserCombinators.{Colour, Elemento, Polenta}

object TADPDrawingApp extends App {

//   Ejemplo de uso del drawing adapter:

  // "C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\parserCombinators\\asd.dibujitos"

  //    TADPDrawingAdapter
//      .forScreen { adapter =>
//        adapter
//          .beginColor(Color.rgb(100, 100, 100))
//          .rectangle((200, 200), (400, 400))
//          .end()
//      }

  val elemento : Elemento = Polenta()("C:\\Users\\guido\\IdeaProjects\\grupo10-2020-2c\\scala\\src\\main\\scala\\tadp\\parserCombinators\\asd.dibujitos")
  println(elemento)


//      TADPDrawingAdapter
//        .forScreen { adapter =>
//          elemento.agregarAdapter(adapter)
//        }

  TADPDrawingAdapter.forInteractiveScreen { (imageDescription, adapter) =>
    Polenta().parsearString(imageDescription).agregarAdapter(adapter)

  }


}
