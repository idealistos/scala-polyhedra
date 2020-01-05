package main.poly

import scala.collection.mutable.ArrayBuffer

object PolyContext {
    val stack = ArrayBuffer[Poly]()
    var iNextVar = 0
    
    def reset() {
        iNextVar = 0
        stack.clear()
    }
    
}