package main.geometry

import main.value.Implicits._


case class MyVec(p: Array[Double]) {
    
    object HashFactors {
        val V = Array(0.238423847, 0.234238498, 0.9458948, 0.92349283, 0.2034829348, 0.23982308, 0.4756587634, 0.328793841723, 0.661524323, 0.8739841748,
                0.13287476475, 0.231476748, 0.8871687634, 0.8567134576, 0.867616345, 0.471283748, 0.012384098, 0.09234892843, 0.987123478, 0.987328748,
                0.987239847, 0.98619875, 0.8726872634, 0.8723387843, 0.987423847, 0.872153984, 0.87343487, 0.9872138, 0.23487982743, 0.0982394839)
    }

    lazy val hash = math.round((this ^ HashFactors.V) * 1000).hashCode
    lazy val projections = p.indices.map(i => this ^ (Bundle.getDirections(p.length)(i).p))
    def unary_- = new MyVec(for (x <- p) yield -x)
    def *(k: Double) = new MyVec(for (x <- p) yield x * k)
    def +(v: MyVec) = new MyVec(for ((x1, x2) <- p zip v.p) yield x1 + x2)
    def -(v: MyVec) = new MyVec(for ((x1, x2) <- p zip v.p) yield x1 - x2)
    def ^(v: Array[Double]) = {
        var s = 0.0
        val n = p.length
        var i = 0
        while (i < n) {
            s += p(i) * v(i)
            i += 1
        }
        s
    }
    
    def ^(v: MyVec): Double = this ^ v.p
    
    def length = math.sqrt(this ^ this)
    def normalize = this * (1.0 / length)
    
    def getOrientedAngle(v1: MyVec, v2: MyVec) = {
        // Angle for the rotation around "this" that brings v2 to the plane spanned by "this" and v1
        val c1 = this ^ v1
        val c2 = this ^ v2
        val c = v1 ^ v2
        math.acos(cs(c1, c2, c).constantValue)
    }
    
    def getPlanarOrientedAngle(v1: MyVec, v2: MyVec) = {
        // Angle for the rotation around "this" that brings v2 to the plane spanned by "this" and v1
        val r1 = v1 - this
        val r2 = v2 - this
        math.acos((r1 ^ r2) / (r1.length * r2.length))
    }
    
    def planarCrossProduct(v: MyVec) = {
        p(0) * v.p(1) - p(1) * v.p(0)
    }
    
    override def equals(that: Any) = that match {
        case x: MyVec => (x.p zip p).forall(y => math.abs(y._1 - y._2) < Tolerance)
        case _ => false
    }
    
    override def hashCode = hash
    
    override def toString = "[" + p.mkString(", ") + "]"
}

object MyVec {
    def apply(n: Int): MyVec = MyVec(Array.fill[Double](n)(0.0))
    def apply(n: Int, i: Int): MyVec = MyVec(Array.range(0, n).map(j => if (j == i) 1.0 else 0.0))
}

