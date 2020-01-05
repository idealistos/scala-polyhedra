package main.poly
import main.value.Value
import main.value.Implicits._
import main.poly.Implicits._

case class IsoscelesTetrahedron(b: Value, s: Value) extends Poly() {
    val face = IsoscelesTriangle(b, s)
    face.base matches face.base
    face.side opposes face.side
    (3 faces) at face.base.vertex
}

case class Tetrahedron(d: Double) extends Poly() {
    val triangle = Triangle(d)
    triangle.edge matches triangle.edge
    (3 faces) at triangle.edge.vertex
}

case class Cube(d: Double) extends Poly() {
    val square = Square(d)
    square.edge matches square.edge
    (3 faces) at square.edge.vertex
}

case class Octahedron(d: Double) extends Poly() {
    val triangle = Triangle(d)
    triangle.edge matches triangle.edge
    (4 faces) at triangle.edge.vertex
}

case class Dodecahedron(d: Double) extends Poly() {
    val pentagon = Pentagon(d)
    pentagon.edge matches pentagon.edge
    (3 faces) at pentagon.edge.vertex
}

case class Icosahedron(d: Double) extends Poly() {
    val triangle = Triangle(d)
    triangle.edge matches triangle.edge
    (5 faces) at triangle.edge.vertex
}

case class Pyramid(b: Double, s: Double) extends Poly() {
    val base = Square(b)
    val side = IsoscelesTriangle(b, s)
    base.edge matches side.base
    side.side matches side.side
    (3 faces) at base.edge.vertex
    (4 faces) at side.side.vertex1
}

case class NSidedPyramid(n: Int, b: Double, s: Double) extends Poly() {
    val base = RegularPolygon(n, b)
    val side = IsoscelesTriangle(b, s)
    base.edge matches side.base
    side.side matches side.side
    (3 faces) at base.edge.vertex
    (n faces) at side.side.vertex1
}

case class Cuboctahedron(d: Double) extends Poly() {
    val square = Square(d)
    val triangle = Triangle(d)
    square.edge matches triangle.edge
    (4 faces) at square.edge.vertex
}

case class RhombicDodecahedron(d: Double) extends Poly() {
    val face = Rhombus(d, x)
    face.side matches face.side
    (3 faces) at face.side.vertex1
    (4 faces) at face.side.vertex2
}

case class RhombicDodecahedron2(d: Value) extends Poly() {
    val angle1 = x(0)
    case class Rhombus1() extends Poly() {
        val side = Side(d)
        (side.vertex1 matches side.vertex1) -> angle1
        (side.vertex2 matches side.vertex2) -> -angle1
    }
    val face = Rhombus1()
    face.side matches face.side
    (3 faces) at face.side.vertex1
    (4 faces) at face.side.vertex2
}

case class TruncatedIcosidodecahedron(d: Double, dCut1: Double, dCut2: Double) extends Poly() {
    val oldHexagon = AlternatingPolygon(6, d, dCut1)
    val oldDecagon = AlternatingPolygon(10, d, dCut2)
    val newRectangle = AlternatingPolygon(4, dCut1, dCut2)
    
    oldHexagon.edge1 matches oldDecagon.edge1
    oldHexagon.edge2 matches newRectangle.edge1
    oldDecagon.edge2 matches newRectangle.edge2
    (3 faces) at oldHexagon.edge1.vertex
}

case class TruncatedRhombicosidodecahedron(dHex: Double, dDec: Double, dHexCut: Double, dDecCut: Double) extends Poly() {
    val hexagon = AlternatingPolygon(6, dHex, dHexCut)
    val decagon = AlternatingPolygon(10, dDec, dDecCut)
    val octagon = CutAlternatingPolygon(8, dHex, dDec, x)
    val trapezoid = IsoscelesTrapezoid(dHexCut, dDecCut, x)
    hexagon.edge1 matches octagon.oldEdge1
    decagon.edge1 matches octagon.oldEdge2
    hexagon.edge2 matches trapezoid.base1
    decagon.edge2 matches trapezoid.base2
    octagon.newEdge matches trapezoid.side
    (3 faces) at hexagon.edge1.vertex
    (3 faces) at decagon.edge1.vertex
}

case class SnubCube(dSquare: Double, dTriangle: Double, dSide: Double) extends Poly() {
    val square = OrientedSquare(dSquare)
    val triangle = OrientedTriangle(dTriangle)
    val sideTriangle = IrregularTriangle(dSquare, dTriangle, dSide)
    square.side matches sideTriangle.sideA
    triangle.side matches sideTriangle.sideB
    sideTriangle.sideC opposes sideTriangle.sideC
    (5 faces) at square.side.vertex1
}

case class SnubDodecahedron(dPentagon: Double, dTriangle: Double, dSide: Double) extends Poly() {
    val pentagon = OrientedPolygon(5, dPentagon)
    val triangle = OrientedTriangle(dTriangle)
    val sideTriangle = IrregularTriangle(dPentagon, dTriangle, dSide)
    pentagon.side matches sideTriangle.sideA
    triangle.side matches sideTriangle.sideB
    sideTriangle.sideC opposes sideTriangle.sideC
    (5 faces) at pentagon.side.vertex1
}

case class Poly3DTest(dA: Value, dB: Value, dC: Value) extends Poly() {
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA.vertex1 matches sideB.vertex1
        sideA.vertex2 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    faceA.sideA opposes faceA.sideA
    faceA.sideB opposes faceA.sideB
    faceA.sideC opposes faceA.sideC
    (3 faces) at faceA.sideA.vertex1
}