package main.poly
import main.poly.Implicits._
import main.value.Implicits._
import main.value.Value


case class HyperTetrahedron(d: Double) extends Poly() {
    val tetrahedron = Tetrahedron(d)
    tetrahedron.triangle matches tetrahedron.triangle
    (3 faces) at tetrahedron.triangle.edge
}

case class HyperCube(d: Double) extends Poly() {
    val cube = Cube(d)
    cube.square matches cube.square
    (3 faces) at cube.square.edge
}

case class NTetra(n: Int, d: Double) extends Poly() {
    val face = if (n == 2) Edge(d) else NTetra(n - 1, d)
    
    face match {
        case edge: Edge => (edge.vertex matches edge.vertex) -> 0.5
        case nTetra: NTetra => (nTetra.face matches nTetra.face) -> 1.0 / n
    }    
}

case class NCube(n: Int, d: Double) extends Poly() {
    val face = if (n == 2) Edge(d) else NCube(n - 1, d)
    
    face match {
        case edge: Edge => (edge.vertex matches edge.vertex) -> 0.0
        case nCube: NCube => (nCube.face matches nCube.face) -> 0.0
    }
}

case class CutHypercube(d: Double) extends Poly() {
    val mainFace = Cuboctahedron(d)
    val newFace = Tetrahedron(d)
    mainFace.square matches mainFace.square
    mainFace.triangle matches newFace.triangle
    (3 faces) at mainFace.square.edge
}

case class HyperPyramidOnCube(b: Double, s: Double) extends Poly() {
    val base = Cube(b)
    val side = Pyramid(b, s)
    base.square matches side.base
    side.side matches side.side
    (3 faces) at base.square.edge
    (3 faces) at side.side.side
}

case class HyperDodecahedron(d: Double) extends Poly() {
    val dodecahedron = Dodecahedron(d)
    dodecahedron.pentagon matches dodecahedron.pentagon
    (3 faces) at dodecahedron.pentagon.edge
}

case class HyperDodecahedron2(d: Double) extends Poly() {
    val dodecahedron = Dodecahedron(d)
    val phi = (1.0 + math.sqrt(5.0)) / 2.0
    val rR = phi * phi * math.sqrt(2)
    val rr1 = phi * phi / (2.0 * math.sqrt(3.0 - phi))
    val rR1 = phi * math.sqrt(3) / 2
    val rr = math.sqrt(rR * rR - rR1 * rR1)
    val cosAngle = (rr1 * rr1 - rr * rr) / (rr1 * rr1 + rr * rr)
   
    (dodecahedron.pentagon matches dodecahedron.pentagon) -> cosAngle
}

case class PrismOnIsoscelesTetrahedron(dA: Value, dB: Value, dC: Value) extends Poly() {
    // Special case of b1a1-a2a2-a1b1 / b1c1-c1b1 / c1a1-a2c2-c2a2-a1c1
    // Edge map:
    //     a -> a1a2 (AB) a2a1 (AB) in hf-AB / a2a1 (AB) a2a1 (AC) in hf-ABC / a1a2 (AC) a1a2 (AB) in hf-ABC
    //     b -> b1b1 (AB) b1b1 (AB) in hf-AB / b1b1 (AB) b1b1 (BC) in hf-ABC / b1b1 (BC) b1b1 (AB) in hf-ABC
    //     c -> c1c1 (AC) c1c1 (BC) in hf-ABC / c1c1 (BC) c1c1 (AC) / c2c2 (AC) c2c2 (AC)
    
    case class FaceAB() extends Polygon(3) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        edgeB.vertex matches sideA.vertex1
        sideA.vertex2 matches sideA.vertex2
    }
    case class FaceBC() extends Polygon(4) {
        val edgeB = Edge(dB)
        val edgeC = Edge(dC)
        edgeB.vertex matches edgeC.vertex
    }
    case class FaceAC() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeC = Edge(dC)
        val edgeC2 = Edge(dC)
        sideA.vertex1 matches edgeC.vertex
        sideA.vertex2 matches edgeC2.vertex
    }
    case class HyperFaceAB() extends Poly() {
        val faceAB = FaceAB()
        faceAB.sideA opposes faceAB.sideA
        faceAB.edgeB matches faceAB.edgeB
        (3 faces) at faceAB.sideA.vertex1
    }
    case class HyperFaceABC() extends Poly() {
        val faceAB = FaceAB()
        val faceAC = FaceAC()
        val faceBC = FaceBC()
        faceAB.sideA matches faceAC.sideA
        faceAB.edgeB matches faceBC.edgeB
        faceAC.edgeC matches faceBC.edgeC
        faceAC.edgeC2 matches faceAC.edgeC2
        (3 faces) at faceAB.sideA.vertex1
        (3 faces) at faceAB.sideA.vertex2
    }
    val hyperFaceAB = HyperFaceAB()
    val hyperFaceABC = HyperFaceABC()
    hyperFaceAB.faceAB matches hyperFaceABC.faceAB
    hyperFaceABC.faceAC opposes hyperFaceABC.faceAC
    hyperFaceABC.faceBC matches hyperFaceABC.faceBC
    (3 faces) at hyperFaceAB.faceAB.sideA
    (3 faces) at hyperFaceAB.faceAB.edgeB
    (3 faces) at hyperFaceABC.faceAC.edgeC
}

case class PrismOnIsoscelesTetrahedron2(m: Int, dA: Value, dP: Value, dQ: Value) extends Poly() {
    // Special case of pa-AA-ap / pq-qp / qX-Xq
    case class FaceAP() extends Polygon(3) {
        val sideA = Side(dA)
        val edgeP = Edge(dP)
        edgeP.vertex matches sideA.vertex1
        sideA.vertex2 matches sideA.vertex2
    }
    case class FacePQ() extends Polygon(4) {
        val edgeP = Edge(dP)
        val edgeQ = Edge(dQ)
        edgeP.vertex matches edgeQ.vertex
    }
    case class FaceAQ() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeQ1 = Edge(dQ)
        val edgeQ2 = Edge(dQ)
        sideA.vertex1 matches edgeQ1.vertex
        sideA.vertex2 matches edgeQ2.vertex
    }
    case class HyperFaceAP() extends Poly() {
        val faceAP = FaceAP()
        faceAP.sideA opposes faceAP.sideA
        faceAP.edgeP matches faceAP.edgeP
        (3 faces) at faceAP.sideA.vertex1
    }
    case class HyperFaceAPQ() extends Poly() {
        val faceAP = FaceAP()
        val faceAQ = FaceAQ()
        val facePQ = FacePQ()
        faceAP.sideA matches faceAQ.sideA
        faceAP.edgeP matches facePQ.edgeP
        faceAQ.edgeQ1 matches facePQ.edgeQ
        faceAQ.edgeQ2 matches faceAQ.edgeQ2
        (3 faces) at faceAP.sideA.vertex1
        (3 faces) at faceAP.sideA.vertex2
    }
    val hyperFaceAP = HyperFaceAP()
    val hyperFaceAPQ = HyperFaceAPQ()
    hyperFaceAP.faceAP matches hyperFaceAPQ.faceAP
    hyperFaceAPQ.faceAQ opposes hyperFaceAPQ.faceAQ
    hyperFaceAPQ.facePQ matches hyperFaceAPQ.facePQ
    (3 faces) at hyperFaceAP.faceAP.sideA
    (3 faces) at hyperFaceAP.faceAP.edgeP
    (3 faces) at hyperFaceAPQ.faceAQ.edgeQ1
}

