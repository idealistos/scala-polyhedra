package main

import main.ValueImplicits._
import main.Implicits._

case class Triangle(d: Value) extends Poly() {
    val edge = Edge(d)
    
    ~~(edge.vertex, edge.vertex) at 0.5
}

case class OrientedTriangle(d: Value) extends Poly() {
    val side = Side(d)
    ~~(side.vertex1, side.vertex2) at 0.5
}

case class Square(d: Value) extends Poly() {
    val edge = Edge(d)
    
    ~~(edge.vertex, edge.vertex) at 0.0
}

case class OrientedSquare(d: Value) extends Poly() {
    val side = Side(d)
    ~~(side.vertex1, side.vertex2) at 0.0
}

case class Polygon(n: Int, d: Value) extends Poly() {
    val edge = Edge(d)
    
    ~~(edge.vertex, edge.vertex) at cn(n)
}

case class AlternatingPolygon(n: Int, d1: Double, d2: Double) extends Poly() {
    val edge1 = Edge(d1)
    val edge2 = Edge(d2)
    ~~(edge1.vertex, edge2.vertex) at cn(n)
}

case class CutAlternatingPolygon(n: Int, dOld1: Value, dOld2: Value, dNew: Value) extends Poly() {
    val oldEdge1 = Edge(dOld1)
    val oldEdge2 = Edge(dOld2)
    val newEdge = Side(dNew)
    ~~(oldEdge1.vertex, newEdge.vertex1) at cn(n)
    ~~(oldEdge2.vertex, newEdge.vertex2) at cn(n)
}

case class IsoscelesTriangle(b: Value, s: Value) extends Poly() {
    val base = Edge(b)
    val side = Side(s)
    ~~(side.vertex1, side.vertex1) at cp(s, s, b)
    ~~(base.vertex, side.vertex2) at cp(s, b, s)
}

case class IrregularTriangle(a: Value, b: Value, c: Value) extends Poly() {
    val sideA = Side(a)
    val sideB = Side(b)
    val sideC = Side(c)
    ~~(sideA.vertex2, sideB.vertex1) at cp(a, b, c)
    ~~(sideB.vertex2, sideC.vertex1) at cp(b, c, a)
    ~~(sideC.vertex2, sideA.vertex1) at cp(c, a, b)
}

case class Rhombus(d: Value, c: Value) extends Poly() {
    val side = Side(d)
    ~~(side.vertex1, side.vertex1) at c
    ~~(side.vertex2, side.vertex2) at -c
}

case class IsoscelesTrapezoid(b1: Value, b2: Value, s: Value) extends Poly() {
    val base1 = Edge(b1)
    val base2 = Edge(b2)
    val side = Side(s)
    
    val c = if (b1 == b2) cp(b2 - b1, s, s) else Constant(0.0)
    ~~(base1.vertex, side.vertex1) at -c
    ~~(base2.vertex, side.vertex2) at c
}

case class IsoscelesTetrahedron(b: Value, s: Value) extends Poly() {
    val face = IsoscelesTriangle(b, s)
    ~~(face.base, face.base) at cs(cp(s, b, s), cp(s, b, s), cp(s, s, b))
    ~#(face.side, face.side) at cs(cp(s, b, s), cp(s, s, b), cp(s, b, s))
}

case class Tetrahedron(d: Double) extends Poly() {
    val triangle = Triangle(d)
    ~~(triangle.edge, triangle.edge) at x
    (3 faces) at triangle.edge.vertex
}

case class HyperTetrahedron(d: Double) extends Poly() {
    val tetrahedron = Tetrahedron(d)
    ~~(tetrahedron.triangle, tetrahedron.triangle) at 0.25
}

case class NTetra(n: Int, d: Double) extends Poly() {
    val face = if (n == 2) Edge(d) else NTetra(n - 1, d)
    
    face match {
        case edge: Edge => ~~(edge.vertex, edge.vertex) at 0.5
        case nTetra: NTetra => ~~(nTetra.face, nTetra.face) at 1.0 / n
    }    
}

case class NCube(n: Int, d: Double) extends Poly() {
    val face = if (n == 2) Edge(d) else NCube(n - 1, d)
    
    face match {
        case edge: Edge => ~~(edge.vertex, edge.vertex) at 0.0
        case nCube: NCube => ~~(nCube.face, nCube.face) at 0.0
    }
}

case class Cuboctahedron(d: Double) extends Poly() {
    val square = Square(d)
    val triangle = Triangle(d)
    ~~(square.edge, triangle.edge) at cs(0.5, 0, -0.5)
}

case class RhombicDodecahedron(d: Double) extends Poly() {
    val face = Rhombus(d, x)
    ~~(face.side, face.side) at y
    (3 faces) at face.side.vertex1
    (4 faces) at face.side.vertex2
}

case class TruncatedIcosidodecahedron(d: Double, dCut1: Double, dCut2: Double) extends Poly() {
    val oldHexagon = AlternatingPolygon(6, d, dCut1)
    val oldDecagon = AlternatingPolygon(10, d, dCut2)
    val newRectangle = AlternatingPolygon(4, dCut1, dCut2)
    ~~(oldHexagon.edge1, oldDecagon.edge1) at cs(cn(6), cn(10), cn(4))
    ~~(oldHexagon.edge2, newRectangle.edge1) at cs(cn(6), cn(4), cn(10))
    ~~(oldDecagon.edge2, newRectangle.edge2) at cs(cn(10), cn(4), cn(6))
}

case class TruncatedRhombicosidodecahedron(dHex: Double, dDec: Double, dHexCut: Double, dDecCut: Double) extends Poly() {
    val hexagon = AlternatingPolygon(6, dHex, dHexCut)
    val decagon = AlternatingPolygon(10, dDec, dDecCut)
    
    val cTrapezoid = (cn(10) - cn(6)) / (2.0 * cn(8))
    val dSide = (dDecCut - dHexCut) / (2.0 * cTrapezoid)
    val octagon = CutAlternatingPolygon(8, dHex, dDec, dSide)
    val trapezoid = IsoscelesTrapezoid(dHexCut, dDecCut, dSide)

    ~~(hexagon.edge1, octagon.oldEdge1) at cs(cn(6), cn(8), -trapezoid.c)
    ~~(decagon.edge1, octagon.oldEdge2) at cs(cn(10), cn(8), trapezoid.c)
    ~~(hexagon.edge2, trapezoid.base1) at cs(cn(6), -trapezoid.c, cn(8))
    ~~(decagon.edge2, trapezoid.base2) at cs(cn(10), trapezoid.c, cn(8))
    ~~(octagon.newEdge, trapezoid.side) at cs(cn(8), trapezoid.c, cn(10))
}

case class SnubCube(dSquare: Double, dTriangle: Double, dSide: Double) extends Poly() {
    val square = OrientedSquare(dSquare)
    val triangle = OrientedTriangle(dTriangle)
    val sideTriangle = IrregularTriangle(dSquare, dTriangle, dSide)
    ~~(square.side, sideTriangle.sideA) at x
    ~~(triangle.side, sideTriangle.sideB) at y
    ~#(sideTriangle.sideC, sideTriangle.sideC) at z
    (5 faces) at square.side.vertex1
}


case class CutHypercube(d: Double) extends Poly() {
    val mainFace = Cuboctahedron(d)
    val newFace = Tetrahedron(d)
    ~~(mainFace.square, mainFace.square) at 0.0
    ~~(mainFace.triangle, newFace.triangle) at 0.0
}
