package main.poly

import main.value.Value
import main.poly.Implicits._

// 4 angles


case class UniformTilingE1(dA: Value, cFaceA1: Value) extends Poly() {
    // A(a1a1)A(a3a2)A(a4a4)A(a2a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        (sideA.vertex1 matches sideA.vertex1) -> cFaceA1
        sideA.vertex2 matches sideA1.vertex1
        sideA1.vertex2 matches sideA1.vertex2
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyF23(dA: Value) extends Poly() {
    // A(a1a1)B(a3a4)A(a2a2)B(a4a3)
    case class FaceA(cFaceA1: Value) extends Polygon(4) {
        val sideA = Side(dA)
        (sideA.vertex1 matches sideA.vertex1) -> cFaceA1
        sideA.vertex2 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        sideA1.vertex1 matches sideA1.vertex2
    }
    val faceA = new FaceA(x(0))
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingF33(dA: Value, cFaceA1: Value) extends Poly() {
    // A(a1a1)B(a3a4)A(a2a2)B(a4a3)
    case class FaceA() extends Polygon(6) {
        val sideA = Side(dA)
        (sideA.vertex1 matches sideA.vertex1) -> cFaceA1
        sideA.vertex2 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        sideA1.vertex1 matches sideA1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingF24(dA: Value, cFaceA1: Value) extends Poly() {
    // A(a1a1)B(a3a4)A(a2a2)B(a4a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        (sideA.vertex1 matches sideA.vertex1) -> cFaceA1
        sideA.vertex2 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        sideA1.vertex1 matches sideA1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyBB323(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(3) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(3) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingBB333(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(3) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(6) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(3) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBB423(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(4) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(3) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingBB424(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(4) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBB523(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(5) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(3) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingBB623(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)C(b3b3)B(b1a3)
    case class FaceA() extends Polygon(6) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeB = Edge(dB)
        edgeA1.vertex matches edgeB.vertex
    }
    case class FaceC() extends Polygon(3) {
        val edgeB1 = Edge(dB)
        edgeB1.vertex matches edgeB1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.edgeA matches faceB.edgeA1
    faceB.edgeB matches faceC.edgeB1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC31(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(3) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC41(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(4) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC51(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(5) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC61(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(6) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC71(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(7) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformPolyBC81(dA: Value, dB: Value) extends Poly() {
    // A(a1a1)B(a3b1)B(b2b2)B(b1a3)
    case class FaceA() extends Polygon(8) {
        val edgeA = Edge(dA)
        edgeA.vertex matches edgeA.vertex
    }
    case class FaceB() extends Polygon(3) {
        val edgeA1 = Edge(dA)
        val sideB = Side(dB)
        edgeA1.vertex matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceB.sideB opposes faceB.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingCC1(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1a1)A(a2b1)A(b2b2)A(b1a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        (sideA.vertex1 matches sideA.vertex1) -> cFaceA1
        sideA.vertex2 matches sideB.vertex1
        sideB.vertex2 matches sideB.vertex2
    }
    val faceA = new FaceA()
    faceA.sideA opposes faceA.sideA
    faceA.sideB opposes faceA.sideB
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF31(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF41(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF51(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(5) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF61(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(6) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF71(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(7) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF81(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b3b2)B(b4a3)
    case class FaceA() extends Polygon(8) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingEE1(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1a4)A(a2b1)A(b3b2)A(b4a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        (sideA.vertex1 matches sideA1.vertex2) -> cFaceA1
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB1.vertex2
        sideB.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceA.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyFF323(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB(cFaceB1: Value) extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB(x(0))
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingFF333(dA: Value, dB: Value, cFaceB1: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(6) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyFF423(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB(cFaceB1: Value) extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB(x(0))
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingFF424(dA: Value, dB: Value, cFaceB1: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(4) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyFF523(dA: Value, dB: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(5) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB(cFaceB1: Value) extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB(x(0))
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingFF623(dA: Value, dB: Value, cFaceB1: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3b4)B(b2a3)
    case class FaceA() extends Polygon(6) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        sideA1.vertex1 matches sideB.vertex2
        (sideA1.vertex2 matches sideB.vertex1) -> cFaceB1
    }
    case class FaceC() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.sideB matches faceC.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF13(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF14(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF15(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(5) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF16(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(6) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF17(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(7) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyEF18(dA: Value, dB: Value) extends Poly() {
    // A(a1a4)A(a2b1)B(b3b4)A(b2a3)
    case class FaceA() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex1 matches sideA1.vertex2
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideB.vertex2
    }
    case class FaceB() extends Polygon(8) {
        val sideB1 = Side(dB)
        sideB1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingAE1(dA: Value, dB: Value) extends Poly() {
    // A(a1b1)A(b1a1)A(a3b2)A(b2a3)
    case class FaceA() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeA = Edge(dA)
        val sideB = Side(dB)
        edgeA.vertex matches sideB.vertex1
        edgeA1.vertex matches sideB.vertex2
    }
    val faceA = new FaceA()
    faceA.edgeA matches faceA.edgeA1
    faceA.sideB matches faceA.sideB
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingCE2(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b1a1)A(a2b2)A(b2a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex2 matches sideB.vertex2
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
    }
    val faceA = new FaceA()
    faceA.sideA opposes faceA.sideA
    faceA.sideB matches faceA.sideB
    (4 faces) at faceA.sideA.vertex1
}



case class UniformTilingEE1V2(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b3a2)A(a4b2)A(b4a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
        sideA.vertex2 matches sideB1.vertex1
        sideA1.vertex2 matches sideB.vertex2
        sideA1.vertex1 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceA.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingFF22(dA: Value, dB: Value, cFaceA1: Value, cFaceB1: Value) extends Poly() {
    // A(a1b1)B(b3a4)A(a2b2)B(b4a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex2 matches sideB.vertex2
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideB1 = Side(dB)
        sideA1.vertex1 matches sideB1.vertex2
        (sideA1.vertex2 matches sideB1.vertex1) -> cFaceB1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceA.sideB matches faceB.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingEE1V3(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b3a2)A(a4b4)A(b2a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
        sideA.vertex2 matches sideB1.vertex1
        sideA1.vertex1 matches sideB.vertex2
        sideA1.vertex2 matches sideB1.vertex2
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceA.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingEE1V4(dA: Value, dB: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b3a4)A(a2b4)A(b2a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideB1 = Side(dB)
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
        sideA.vertex2 matches sideB1.vertex2
        sideA1.vertex1 matches sideB.vertex2
        sideA1.vertex2 matches sideB1.vertex1
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB matches faceA.sideB1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyBBF312(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3c1)B(c3a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val edgeB = Edge(dB)
        val edgeC1 = Edge(dC)
        sideA1.vertex2 matches edgeB.vertex
        sideA1.vertex1 matches edgeC1.vertex
    }
    case class FaceC() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        val edgeC = Edge(dC)
        edgeB1.vertex matches edgeC.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.edgeB matches faceC.edgeB1
    faceC.edgeC matches faceB.edgeC1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingBBF313(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3c1)B(c3a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val edgeB = Edge(dB)
        val edgeC1 = Edge(dC)
        sideA1.vertex2 matches edgeB.vertex
        sideA1.vertex1 matches edgeC1.vertex
    }
    case class FaceC() extends Polygon(6) {
        val edgeB1 = Edge(dB)
        val edgeC = Edge(dC)
        edgeB1.vertex matches edgeC.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.edgeB matches faceC.edgeB1
    faceC.edgeC matches faceB.edgeC1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingBBF412(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)C(b3c1)B(c3a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val edgeB = Edge(dB)
        val edgeC1 = Edge(dC)
        sideA1.vertex2 matches edgeB.vertex
        sideA1.vertex1 matches edgeC1.vertex
    }
    case class FaceC() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        val edgeC = Edge(dC)
        edgeB1.vertex matches edgeC.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA matches faceB.sideA1
    faceB.edgeB matches faceC.edgeB1
    faceC.edgeC matches faceB.edgeC1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF31(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(3) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF41(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF51(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(5) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF61(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(6) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF71(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(7) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformPolyCCF81(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1a2)B(a4b1)B(b2c1)B(c2a3)
    case class FaceA() extends Polygon(8) {
        val sideA = Side(dA)
        sideA.vertex1 matches sideA.vertex2
    }
    case class FaceB() extends Polygon(3) {
        val sideA1 = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        sideA1.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceB.sideB opposes faceB.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingCCE1(dA: Value, dB: Value, dC: Value, cFaceA1: Value) extends Poly() {
    // A(a1a4)A(a2b1)A(b2c1)A(c2a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        (sideA.vertex1 matches sideA1.vertex2) -> cFaceA1
        sideA.vertex2 matches sideB.vertex1
        sideA1.vertex1 matches sideC.vertex2
        sideB.vertex2 matches sideC.vertex1
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB opposes faceA.sideB
    faceA.sideC opposes faceA.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingAAB22(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1b1)A(b1a1)B(a3c1)B(c1a3)
    case class FaceA() extends Polygon(4) {
        val edgeA = Edge(dA)
        val edgeB = Edge(dB)
        edgeA.vertex matches edgeB.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeC = Edge(dC)
        edgeA1.vertex matches edgeC.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.edgeA matches faceB.edgeA1
    faceA.edgeB matches faceA.edgeB
    faceB.edgeC matches faceB.edgeC
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingAAC1(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1b1)A(b1a1)A(a2c1)A(c1a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        val edgeC = Edge(dC)
        sideA.vertex1 matches edgeB.vertex
        sideA.vertex2 matches edgeC.vertex
    }
    val faceA = new FaceA()
    faceA.sideA opposes faceA.sideA
    faceA.edgeB matches faceA.edgeB
    faceA.edgeC matches faceA.edgeC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingAAF11(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1b1)A(b3a2)B(a4c1)B(c3a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        val edgeB1 = Edge(dB)
        sideA.vertex1 matches edgeB.vertex
        sideA.vertex2 matches edgeB1.vertex
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val edgeC1 = Edge(dC)
        val edgeC = Edge(dC)
        sideA1.vertex2 matches edgeC.vertex
        sideA1.vertex1 matches edgeC1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceA.edgeB matches faceA.edgeB1
    faceB.edgeC matches faceB.edgeC1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingBBF11(dA: Value, dB: Value, dC: Value) extends Poly() {
    // A(a1b1)B(b3a4)A(a2c1)B(c3a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        val edgeC = Edge(dC)
        sideA.vertex1 matches edgeB.vertex
        sideA.vertex2 matches edgeC.vertex
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val edgeB1 = Edge(dB)
        val edgeC1 = Edge(dC)
        sideA1.vertex2 matches edgeB1.vertex
        sideA1.vertex1 matches edgeC1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceA.edgeB matches faceB.edgeB1
    faceA.edgeC matches faceB.edgeC1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingCCF22(dA: Value, dB: Value, dC: Value, cFaceA1: Value, cFaceB1: Value) extends Poly() {
    // A(a1b1)A(b2a2)B(a4c1)B(c2a3)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        sideA.vertex2 matches sideB.vertex2
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
    }
    case class FaceB() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideC = Side(dC)
        sideA1.vertex1 matches sideC.vertex2
        (sideA1.vertex2 matches sideC.vertex1) -> cFaceB1
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA matches faceB.sideA1
    faceA.sideB opposes faceA.sideB
    faceB.sideC opposes faceB.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingCCE1V2(dA: Value, dB: Value, dC: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b2a4)A(a2c1)A(c2a3)
    case class FaceA() extends Polygon(4) {
        val sideA1 = Side(dA)
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
        sideA.vertex2 matches sideC.vertex1
        sideA1.vertex2 matches sideB.vertex2
        sideA1.vertex1 matches sideC.vertex2
    }
    val faceA = new FaceA()
    faceA.sideA matches faceA.sideA1
    faceA.sideB opposes faceA.sideB
    faceA.sideC opposes faceA.sideC
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingBBBB2222(dA: Value, dB: Value, dC: Value, dD: Value) extends Poly() {
    // A(a1b1)B(b3c1)C(c3d1)D(d3a3)
    case class FaceA() extends Polygon(4) {
        val edgeA = Edge(dA)
        val edgeB = Edge(dB)
        edgeA.vertex matches edgeB.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        val edgeC = Edge(dC)
        edgeB1.vertex matches edgeC.vertex
    }
    case class FaceC() extends Polygon(4) {
        val edgeC1 = Edge(dC)
        val edgeD = Edge(dD)
        edgeC1.vertex matches edgeD.vertex
    }
    case class FaceD() extends Polygon(4) {
        val edgeA1 = Edge(dA)
        val edgeD1 = Edge(dD)
        edgeA1.vertex matches edgeD1.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    val faceD = new FaceD()
    faceA.edgeA matches faceD.edgeA1
    faceA.edgeB matches faceB.edgeB1
    faceB.edgeC matches faceC.edgeC1
    faceC.edgeD matches faceD.edgeD1
    (4 faces) at faceA.edgeA.vertex
}

case class UniformTilingBBBC122(dA: Value, dB: Value, dC: Value, dD: Value) extends Poly() {
    // A(a1b1)B(b3c1)C(c3d1)A(d3a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        val edgeD1 = Edge(dD)
        sideA.vertex1 matches edgeB.vertex
        sideA.vertex2 matches edgeD1.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        val edgeC = Edge(dC)
        edgeB1.vertex matches edgeC.vertex
    }
    case class FaceC() extends Polygon(4) {
        val edgeC1 = Edge(dC)
        val edgeD = Edge(dD)
        edgeC1.vertex matches edgeD.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    val faceC = new FaceC()
    faceA.sideA opposes faceA.sideA
    faceA.edgeB matches faceB.edgeB1
    faceB.edgeC matches faceC.edgeC1
    faceC.edgeD matches faceA.edgeD1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingBBCC11(dA: Value, dB: Value, dC: Value, dD: Value) extends Poly() {
    // A(a1b1)B(b3c1)B(c2d1)A(d3a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val edgeB = Edge(dB)
        val edgeD1 = Edge(dD)
        sideA.vertex1 matches edgeB.vertex
        sideA.vertex2 matches edgeD1.vertex
    }
    case class FaceB() extends Polygon(4) {
        val edgeB1 = Edge(dB)
        val sideC = Side(dC)
        val edgeD = Edge(dD)
        edgeB1.vertex matches sideC.vertex1
        sideC.vertex2 matches edgeD.vertex
    }
    val faceA = new FaceA()
    val faceB = new FaceB()
    faceA.sideA opposes faceA.sideA
    faceA.edgeB matches faceB.edgeB1
    faceB.sideC opposes faceB.sideC
    faceB.edgeD matches faceA.edgeD1
    (4 faces) at faceA.sideA.vertex1
}

case class UniformTilingCCCC1(dA: Value, dB: Value, dC: Value, dD: Value, cFaceA1: Value) extends Poly() {
    // A(a1b1)A(b2c1)A(c2d1)A(d2a2)
    case class FaceA() extends Polygon(4) {
        val sideA = Side(dA)
        val sideB = Side(dB)
        val sideC = Side(dC)
        val sideD = Side(dD)
        (sideA.vertex1 matches sideB.vertex1) -> cFaceA1
        sideA.vertex2 matches sideD.vertex2
        sideB.vertex2 matches sideC.vertex1
        sideC.vertex2 matches sideD.vertex1
    }
    val faceA = new FaceA()
    faceA.sideA opposes faceA.sideA
    faceA.sideB opposes faceA.sideB
    faceA.sideC opposes faceA.sideC
    faceA.sideD opposes faceA.sideD
    (4 faces) at faceA.sideA.vertex1
}

object PolyWrapperSamples4 {
    // 62 found
    val wrappers = Vector(
            PolyWrapper("UniformTilingE1", 1, 1, v => UniformTilingE1(v(0), v(1))),
            PolyWrapper("UniformPolyF23", 1, 0, v => UniformPolyF23(v(0))),
            PolyWrapper("UniformTilingF33", 1, 1, v => UniformTilingF33(v(0), v(1))),
            PolyWrapper("UniformTilingF24", 1, 1, v => UniformTilingF24(v(0), v(1))),
            PolyWrapper("UniformPolyBB323", 2, 0, v => UniformPolyBB323(v(0), v(1))),
            PolyWrapper("UniformTilingBB333", 2, 0, v => UniformTilingBB333(v(0), v(1))),
            PolyWrapper("UniformPolyBB423", 2, 0, v => UniformPolyBB423(v(0), v(1))),
            PolyWrapper("UniformTilingBB424", 2, 0, v => UniformTilingBB424(v(0), v(1))),
            PolyWrapper("UniformPolyBB523", 2, 0, v => UniformPolyBB523(v(0), v(1))),
            PolyWrapper("UniformTilingBB623", 2, 0, v => UniformTilingBB623(v(0), v(1))),
            PolyWrapper("UniformPolyBC31", 2, 0, v => UniformPolyBC31(v(0), v(1))),
            PolyWrapper("UniformPolyBC41", 2, 0, v => UniformPolyBC41(v(0), v(1))),
            PolyWrapper("UniformPolyBC51", 2, 0, v => UniformPolyBC51(v(0), v(1))),
            PolyWrapper("UniformPolyBC61", 2, 0, v => UniformPolyBC61(v(0), v(1))),
            PolyWrapper("UniformPolyBC71", 2, 0, v => UniformPolyBC71(v(0), v(1))),
            PolyWrapper("UniformPolyBC81", 2, 0, v => UniformPolyBC81(v(0), v(1))),
            PolyWrapper("UniformTilingCC1", 2, 1, v => UniformTilingCC1(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyEF31", 2, 0, v => UniformPolyEF31(v(0), v(1))),
            PolyWrapper("UniformPolyEF41", 2, 0, v => UniformPolyEF41(v(0), v(1))),
            PolyWrapper("UniformPolyEF51", 2, 0, v => UniformPolyEF51(v(0), v(1))),
            PolyWrapper("UniformPolyEF61", 2, 0, v => UniformPolyEF61(v(0), v(1))),
            PolyWrapper("UniformPolyEF71", 2, 0, v => UniformPolyEF71(v(0), v(1))),
            PolyWrapper("UniformPolyEF81", 2, 0, v => UniformPolyEF81(v(0), v(1))),
            PolyWrapper("UniformTilingEE1", 2, 1, v => UniformTilingEE1(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyFF323", 2, 0, v => UniformPolyFF323(v(0), v(1))),
            PolyWrapper("UniformTilingFF333", 2, 1, v => UniformTilingFF333(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyFF423", 2, 0, v => UniformPolyFF423(v(0), v(1))),
            PolyWrapper("UniformTilingFF424", 2, 1, v => UniformTilingFF424(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyFF523", 2, 0, v => UniformPolyFF523(v(0), v(1))),
            PolyWrapper("UniformTilingFF623", 2, 1, v => UniformTilingFF623(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyEF13", 2, 0, v => UniformPolyEF13(v(0), v(1))),
            PolyWrapper("UniformPolyEF14", 2, 0, v => UniformPolyEF14(v(0), v(1))),
            PolyWrapper("UniformPolyEF15", 2, 0, v => UniformPolyEF15(v(0), v(1))),
            PolyWrapper("UniformPolyEF16", 2, 0, v => UniformPolyEF16(v(0), v(1))),
            PolyWrapper("UniformPolyEF17", 2, 0, v => UniformPolyEF17(v(0), v(1))),
            PolyWrapper("UniformPolyEF18", 2, 0, v => UniformPolyEF18(v(0), v(1))),
            PolyWrapper("UniformTilingAE1", 2, 0, v => UniformTilingAE1(v(0), v(1))),
            PolyWrapper("UniformTilingCE2", 2, 1, v => UniformTilingCE2(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingEE1V2", 2, 1, v => UniformTilingEE1V2(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingFF22", 2, 2, v => UniformTilingFF22(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingEE1V3", 2, 1, v => UniformTilingEE1V3(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingEE1V4", 2, 1, v => UniformTilingEE1V4(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyBBF312", 3, 0, v => UniformPolyBBF312(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingBBF313", 3, 0, v => UniformTilingBBF313(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingBBF412", 3, 0, v => UniformTilingBBF412(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF31", 3, 0, v => UniformPolyCCF31(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF41", 3, 0, v => UniformPolyCCF41(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF51", 3, 0, v => UniformPolyCCF51(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF61", 3, 0, v => UniformPolyCCF61(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF71", 3, 0, v => UniformPolyCCF71(v(0), v(1), v(2))),
            PolyWrapper("UniformPolyCCF81", 3, 0, v => UniformPolyCCF81(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingCCE1", 3, 1, v => UniformTilingCCE1(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingAAB22", 3, 0, v => UniformTilingAAB22(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingAAC1", 3, 0, v => UniformTilingAAC1(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingAAF11", 3, 0, v => UniformTilingAAF11(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingBBF11", 3, 0, v => UniformTilingBBF11(v(0), v(1), v(2))),
            PolyWrapper("UniformTilingCCF22", 3, 2, v => UniformTilingCCF22(v(0), v(1), v(2), v(3), v(4))),
            PolyWrapper("UniformTilingCCE1V2", 3, 1, v => UniformTilingCCE1V2(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingBBBB2222", 4, 0, v => UniformTilingBBBB2222(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingBBBC122", 4, 0, v => UniformTilingBBBC122(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingBBCC11", 4, 0, v => UniformTilingBBCC11(v(0), v(1), v(2), v(3))),
            PolyWrapper("UniformTilingCCCC1", 4, 1, v => UniformTilingCCCC1(v(0), v(1), v(2), v(3), v(4))),
    )
}
