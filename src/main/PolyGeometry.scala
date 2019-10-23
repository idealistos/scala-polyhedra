package main

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, HashSet => MHashSet}
import scala.runtime.ScalaRunTime
import scala.math.Ordering.Implicits._


object HashFactors {
    val V = Array(0.238423847, 0.234238498, 0.9458948, 0.92349283, 0.2034829348, 0.23982308, 0.4756587634, 0.328793841723, 0.661524323, 0.8739841748,
            0.13287476475, 0.231476748, 0.8871687634, 0.8567134576, 0.867616345, 0.471283748, 0.012384098, 0.09234892843, 0.987123478, 0.987328748,
            0.987239847, 0.98619875, 0.8726872634, 0.8723387843, 0.987423847, 0.872153984, 0.87343487, 0.9872138, 0.23487982743, 0.0982394839)
}

case class MyVec(p: Array[Double]) {
    val hash = math.round((this ^ HashFactors.V) * 1000).hashCode
    def unary_- = new MyVec(for (x <- p) yield -x)
    def *(k: Double) = new MyVec(for (x <- p) yield x * k)
    def +(v: MyVec) = new MyVec(for ((x1, x2) <- p zip v.p) yield x1 + x2)
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
    
    def length = math.sqrt(this ^ this.p)
    
    override def equals(that: Any) = that match {
        case x: MyVec => x.hash == hash && (x.p zip p).forall(y => math.abs(y._1 - y._2) < 1e-10)
        case _ => false
    }
    
    override def hashCode = hash
    
    override def toString = "[" + p.mkString(", ") + "]"
}

object MyVec {
    def apply(n: Int): MyVec = MyVec(Array.fill[Double](n)(0.0))
    def apply(n: Int, i: Int): MyVec = MyVec(Array.range(0, n).map(j => if (j == i) 1.0 else 0.0))
}

class Bundle(val v: Vertex, val point: MyVec, val basis: Array[MyVec]) {
    
    override def hashCode = {
        var hash = point.hash
        for (vec <- basis) {
            hash = hash * 31 + vec.hash
        }
        hash
    }
    
    override def equals(that: Any) = that match {
        case x: Bundle => (x.v eq v) && (x.point == point) && (x.basis zip basis).forall(y => y._1 == y._2)
        case _ => false
    }
    
    def shift(i: Int): Bundle = {
        val vNew = v.nb(i)
        val value = v.nbValue(i).constantValue
        if (i == 0) {
            // Shift the vertex by "value" along basis[0] (edge vector), turn basis[0] around
            new Bundle(vNew, point + basis(0) * value, (-basis(0)) +: basis.drop(1))
        } else {
            // Modify basis[i - 1] (e.g., edge normal for i = 2) and basis[i] (e.g., face normal)
            // v1' = v1 * cos(a) + v2 * sin(a), v2' = -v2 * cos(a) + v1 * sin(a)
            val c = value
            val s = math.sqrt(1.0 - c * c)
            val v1 = basis(i - 1) * c + basis(i) * s
            val v2 = basis(i) * (-c) + basis(i - 1) * s
            new Bundle(vNew, point, (basis.take(i - 1) :+ v1 :+ v2) ++ basis.drop(i + 1))
        }
    }
    
    override def toString = "" + v + " at " + point
}

object Bundle {
    def apply(poly: Poly) = {
        val v = poly.vList(0)
        new Bundle(v, MyVec(v.rank), Array.range(0, v.rank).map(i => MyVec(v.rank, i)))
    }
}

case class PolyGeometry(poly: Poly) {
    val pointMap = MHashMap[MyVec, Int]()
    val edgeMap = MHashSet[(Int, Int)]()
    val faceMap = MHashMap[MyVec, MHashSet[(Int, Int)]]()
    val faceTypeMap = MHashMap[MyVec, String]()
    val n = poly.vList(0).rank
    
    lazy val points = {
        val array = Array.fill[MyVec](pointMap.size)(null)
        for ((point, iPoint) <- pointMap) {
            array(iPoint) = point
        }
        array
    }
    
    def registerPoint(point: MyVec) = pointMap.getOrElseUpdate(point, pointMap.size)
    
    def registerEdge(iPoint1: Int, iPoint2: Int) {
        if (iPoint1 < iPoint2) {
            edgeMap.add((iPoint1, iPoint2))
        }
    }

    def registerFaceEdge(normal: MyVec, iPoint1: Int, iPoint2: Int) {
        val iPointMin = math.min(iPoint1, iPoint2)
        val iPointMax = math.max(iPoint1, iPoint2)
        val edges = faceMap.getOrElseUpdate(normal, MHashSet())
        edges.add((iPointMin, iPointMax))
    }
    
    def registerFaceType(normal: MyVec, faceName: String) {
        faceTypeMap(normal) = faceName
    }
        
    def getFace(edges: MHashSet[(Int, Int)], iPointBuffer: Array[Array[Int]]) = {
        for ((i1, i2) <- edges) {
            for (i <- 0 until 2) {
                iPointBuffer(i1)(i) = -1
                iPointBuffer(i2)(i) = -1
            }
        }
        var i0 = -1
        for ((i1, i2) <- edges) {
            if (iPointBuffer(i1)(0) == -1) {
                iPointBuffer(i1)(0) = i2
            } else {
                iPointBuffer(i1)(1) = i2
            }
            if (iPointBuffer(i2)(0) == -1) {
                iPointBuffer(i2)(0) = i1
            } else {
                iPointBuffer(i2)(1) = i1
            }
            i0 = i1
        }
        val iPoints = ArrayBuffer[Int]()
        var i = i0
        var iNext = 0
        var iPrev = -1
        do {
            iNext = iPointBuffer(i)(1)
            if (iNext == iPrev && iNext != -1) {
                iNext = iPointBuffer(i)(0)
            }
            iPrev = i
            i = iNext
        } while (i != i0 && iNext != -1)

        i0 = iPrev
        iPrev = -1
        i = i0
        do {
            var iNext = iPointBuffer(i)(0)
            if (iNext == iPrev) {
                iNext = iPointBuffer(i)(1)
            }
            if (iPrev == -1 || i != i0) {
                iPoints.append(i)
            }
            iPrev = i
            i = iNext
        } while (i != i0 && i != -1)
        iPoints.toVector
    }
    
    def calculate() {
        val queue = ArrayBuffer[Bundle]()
        val visited = MHashSet[Bundle]()
        
        def register(bundle: Bundle) {
            if (visited.add(bundle)) {
                queue.append(bundle)
            }
        }
        
        register(Bundle(poly))
        registerPoint(queue(0).point)
        var i = 0
        while (i < queue.length) {
            for (j <- 0 until n) {
                val newBundle = queue(i).shift(j)
                register(newBundle)
                if (j == 0) {
                    val iOldPoint = pointMap(queue(i).point)
                    val iNewPoint = registerPoint(newBundle.point)
                    registerEdge(iOldPoint, iNewPoint)
                    registerFaceEdge(newBundle.basis(n - 1), iOldPoint, iNewPoint)
                }
                if (j == n - 1) {
                    registerFaceType(newBundle.basis(n - 1), newBundle.v.getFaceParent.name)
                }
            }
            i += 1
        }
    }
    
    calculate()
    
}
