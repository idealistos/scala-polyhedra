package main.poly

import main.value.Variable

object Implicits {
    implicit def toCountByAxis(count: Int) = new CountByAxis(count)
    implicit def toPolyCopy[P <: Poly](poly: P) = new PolyCopy[P](poly)    
    def x(iRelative: Int) = {
        PolyContext.stack.last.localVars.getOrElseUpdate(iRelative, {
            val i = PolyContext.iNextVar
            PolyContext.iNextVar += 1
            Variable(i)
        })
    }
    def x: Variable = x(0)
    def y: Variable = x(1)
    def z: Variable = x(2)
}