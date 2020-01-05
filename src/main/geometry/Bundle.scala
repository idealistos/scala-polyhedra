package main.geometry

import scala.collection.mutable.ArrayBuffer

import main.poly.Poly
import main.poly.Vertex
import main.drawing.PolyDrawingInfo
import main.value.Implicits._

class Bundle(val v: Vertex, val basis: Array[MyVec], val projections: Array[Double]) extends Ordered[Bundle] {
    // basis[0]: point, basis[1].. basis[n]: basis vectors (edge, edge normal,.., normal to the face)
    
    def setComponent(i: Int, component: MyVec) {
        basis(i) = component
        addToProjections(i, component)        
    }
    
    def addToProjections(i: Int, component: MyVec) {
        val n = v.rank
        if (i != v.rank) {
            for (j <- 0 until n) {
                projections((i + j) % n) += component.projections(j) 
            }
        }
    }
    
    def removeProjection(i: Int) {
        val n = v.rank
        if (i != v.rank) {
            for (j <- 0 until n) {
                projections((i + j) % n) -= basis(i).projections(j)
            }
        }
    }
    
    def hasCloseProjection(bundle: Bundle): Boolean = {
        for ((p1, p2) <- projections zip bundle.projections) {
            if (math.abs(p1 - p2) > 10 * Tolerance) {
                return false
            }
        }
        return true
    }
    
    def shiftedVertex = basis(0) + basis(1) * v.nbValue(0).constantValue
    
    def shift(i: Int): Bundle = {
        val vNew = v.nb(i)
        val value = v.nbValue(i).constantValue
        val bundle = new Bundle(vNew, basis.clone, projections.clone)
        if (i == 0) {
            // Shift the vertex by "value" along basis[0] (edge vector), turn basis[0] around
            bundle.removeProjection(0)
            bundle.removeProjection(1)
            bundle.setComponent(0, shiftedVertex)
            bundle.setComponent(1, -basis(1))
        } else {
            // Modify basis[i - 1] (e.g., edge normal for i = 2) and basis[i] (e.g., face normal)
            // v1' = v1 * cos(a) + v2 * sin(a), v2' = -v2 * cos(a) + v1 * sin(a)
            val c = value
            val s = math.sqrt(1.0 - c * c)
            bundle.removeProjection(i)
            bundle.removeProjection(i + 1)
            bundle.setComponent(i, basis(i) * c + basis(i + 1) * s)
            bundle.setComponent(i + 1, basis(i + 1) * (-c) + basis(i) * s)
        }
        bundle
    }
    
    override def equals(that: Any) = that match {
        case x: Bundle => (x.v eq v) && hasCloseProjection(x) && (x.basis zip basis).forall(y => y._1 == y._2)
        case _ => false
    }
    
    override def compare(bundle: Bundle) = Ordering.Double.compare(projections(0), bundle.projections(0))
    override def toString = "" + v + " at " + basis(0)
}

object Bundle {
    val directionsList = new ArrayBuffer[Array[MyVec]]()
    
    def apply(poly: Poly) = {
        val v = poly.vList(0)
        val bundle = new Bundle(v, Array.fill(v.rank + 1)(null), Array.fill(v.rank)(0.0))
        bundle.setComponent(0, MyVec(v.rank))
        for (i <- 0 until v.rank) {
            bundle.setComponent(i + 1, MyVec(v.rank, i))
        }
        bundle
    }
    
    def getDirections(n: Int) = {
        if (n >= directionsList.size) {
            val nNext = directionsList.size
            for (i <- nNext to n) {
                if (i == 0) {
                    directionsList += null
                } else {
                    directionsList += PolyDrawingInfo.getRandomRotationMatrix(i).map(v => MyVec(v))
                }
            }
        }
        directionsList(n)
    }
    
}
