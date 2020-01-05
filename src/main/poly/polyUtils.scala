package main.poly

import main.value.Value

final case class NotOpposable() extends Exception

class CountByAxis(count: Int) {

    class Inner(facesDescriptor: CountByAxis) {
        def at(axisPoly: Poly) {
            val owner = axisPoly.o.o.o
            owner.setCountByAxis(axisPoly, count)
        }
    }

    def faces() = new Inner(this)
}

class PolyCopy[P <: Poly](poly: P) {
    def matches(poly1: P) = PolyContext.stack.last.matchesAtAngle(poly, poly1)
    def opposes(poly1: P) = PolyContext.stack.last.opposesAtAngle(poly, poly1)
}


abstract class InnerPoly(val vars: Array[Value]) extends Poly() {
    override def canEqual(o: Any) = o.isInstanceOf[InnerPoly]
    override def productArity = vars.length
    override def productElement(n: Int) = vars(n)
}

object InitialBasisMatrix {
    val value = for (i <- Array.range(0, 3)) yield for (j <- Array.range(0, 3)) yield if (i == j) 1.0 else 0.0
}

case class PolygonElements(sides: Array[Value], cosines: Array[Value])

case class PolyWrapper(
        polyClassName: String,
        sideParameterCount: Int,
        cosineParameterCount: Int,
        polyFunction: Array[Value] => Poly,
)

object PolyWrapperSamples {
    val wrappers = Array[PolyWrapper]()
}
