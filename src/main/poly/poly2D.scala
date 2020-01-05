package main.poly

import main.value.Value
import main.value.Constant
import main.value.Implicits._
import main.poly.Implicits._

case class FaceAP(dA: Value, dP: Value) extends Polygon(3) {
    val sideA = Side(dA)
    val edgeP = Edge(dP)
    edgeP.vertex matches sideA.vertex1
    sideA.vertex2 matches sideA.vertex2
}

case class Triangle(d: Value) extends Poly() {
    val edge = Edge(d)
    (edge.vertex matches edge.vertex) -> 0.5
}

case class OrientedTriangle(d: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex2) -> 0.5
}

case class Square(d: Value) extends Poly() {
    val edge = Edge(d)
    (edge.vertex matches edge.vertex) -> 0.0
}

case class OrientedSquare(d: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex2) -> 0.0
}

case class Pentagon(d: Value) extends Poly() {
    val edge = Edge(d)
    (edge.vertex matches edge.vertex) -> cn(5)
}

case class OrientedPentagon(d: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex1) -> cn(5)
}

case class Hexagon(d: Value) extends Poly() {
    val edge = Edge(d)
    (edge.vertex matches edge.vertex) -> -0.5
}

case class OrientedHexagon(d: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex1) -> -0.5
}

case class RegularPolygon(n: Int, d: Value) extends Poly() {
    val edge = Edge(d)
    (edge.vertex matches edge.vertex) -> cn(n)
}

case class OrientedPolygon(n: Int, d: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex2) -> cn(n)
}

case class AlternatingPolygon(n: Int, d1: Double, d2: Double) extends Poly() {
    val edge1 = Edge(d1)
    val edge2 = Edge(d2)
    (edge1.vertex matches edge2.vertex) -> cn(n)
}

case class CutAlternatingPolygon(n: Int, dOld1: Value, dOld2: Value, dNew: Value) extends Poly() {
    val oldEdge1 = Edge(dOld1)
    val oldEdge2 = Edge(dOld2)
    val newEdge = Side(dNew)
    (oldEdge1.vertex matches newEdge.vertex1) -> cn(n)
    (oldEdge2.vertex matches newEdge.vertex2) -> cn(n)
}

case class IsoscelesTriangle(b: Value, s: Value) extends Poly() {
    val base = Edge(b)
    val side = Side(s)
    (side.vertex1 matches side.vertex1) -> cp(s, s, b)
    (base.vertex matches side.vertex2) -> cp(s, b, s)
}

case class IrregularTriangle(a: Value, b: Value, c: Value) extends Poly() {
    val sideA = Side(a)
    val sideB = Side(b)
    val sideC = Side(c)
    (sideA.vertex2 matches sideB.vertex1) -> cp(a, b, c)
    (sideB.vertex2 matches sideC.vertex1) -> cp(b, c, a)
    (sideC.vertex2 matches sideA.vertex1) -> cp(c, a, b)
}

case class Rhombus(d: Value, c: Value) extends Poly() {
    val side = Side(d)
    (side.vertex1 matches side.vertex1) -> c
    (side.vertex2 matches side.vertex2) -> -c
}

case class IsoscelesTrapezoid(b1: Value, b2: Value, s: Value) extends Poly() {
    val base1 = Edge(b1)
    val base2 = Edge(b2)
    val side = Side(s)
    
    val c = if (b1 == b2) Constant(0.0) else cp(b2 - b1, s, s)
    (base1.vertex matches side.vertex1) -> -c
    (base2.vertex matches side.vertex2) -> c
}
