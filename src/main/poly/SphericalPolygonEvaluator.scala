package main.poly

import scala.runtime.ScalaRunTime
import scala.collection.mutable.ArrayBuffer
import main.value.Computed
import main.value.Value
import main.geometry.MyVec
import main.value.Implicits._

case class SphericalPolygonEvaluator(sides: Array[Value], cosines: Array[Value], varCount: Int) extends PolygonEvaluator {

    def sumSideAngles = sides.map(x => math.acos(x.constantValue)).sum
    
    def shiftToNextVertex(basis: Array[Array[Computed]], side: Computed) {
        // New basis[0] (V): V = v cos alpha + e sin alpha
        // New basis[1] (E): E = e cos alpha - v sin alpha
        // V'[j] = v'[j] c + v[j] c' + e'[j] sqrt(1 - c^2) - e[j] c' c / sqrt(1 - c^2)
        // E'[j] = e'[j] c + e[j] c' - v'[j] sqrt(1 - c^2) + v[j] c' c / sqrt(1 - c^2)
        val newV = for (i <- Array.range(0, 3)) yield basis(0)(i).timesCos(side) + basis(1)(i).timesSin(side)
        val newE = for (i <- Array.range(0, 3)) yield basis(1)(i).timesCos(side) - basis(0)(i).timesSin(side)
        basis(0) = newV
        basis(1) = newE
    }
    
    def rotateSideNormal(basis: Array[Array[Computed]], cosine: Computed) {
        // New basis[1] (E): E = -e cos alpha + n sin alpha
        // New basis[2] (N): N = -n cos alpha - e sin alpha
        val newE = for (i <- Array.range(0, 3)) yield -basis(1)(i).timesCos(cosine) + basis(2)(i).timesSin(cosine)
        val newN = for (i <- Array.range(0, 3)) yield -basis(2)(i).timesCos(cosine) - basis(1)(i).timesSin(cosine)
        basis(1) = newE
        basis(2) = newN
    }
    
    def getLastAngleMismatch(normal: Array[Computed], cosine: Computed) = {
        // (n . (0, 0, 1) + cos(alpha))^2 and its derivatives - no:
        // sin(arccos c1 - arccos c2), with c1 = -cosine and c2 = n . (0, 0, 1)
        // Expands to c2 * sqrt(1 - c1^2) - c1 * sqrt(1 - c2^2) 
        
        val Computed(c1, dc1) = -cosine
        val Computed(c2, dc2) = normal(2)
        val s1 = math.sqrt(1 - c1 * c1)
        val s2 = math.sqrt(1 - c2 * c2)
        val value = s1 * c2 - s2 * c1
        val dfs = for (i <- Array.range(0, varCount)) yield {
            2.0 * value * (dc1(i) * (-c1 * c2 / s1  - s2) + dc2(i) * (s1 + c1 * c2 / s2))
        }
        Computed(value * value, dfs)
        
//        val Computed(c, dc) = cosine
//        val Computed(n2, dn2) = normal(2)
//        println("Objective (c): " + (n2 + c) * (n2 + c))
//        Computed((n2 + c) * (n2 + c), for (i <- varIndices) yield 2.0 * (n2 + c) * (dn2(i) + dc(i)))
    }

    def getPoints = {
        val points = ArrayBuffer[MyVec]()
        val basis = computedInitialBasis
        for (i <- sides.indices) {
            points += MyVec(basis(0).map(_.f))
            shiftToNextVertex(basis, computedSides(i))
            if (i < sides.length - 1) {
                rotateSideNormal(basis, computedCosines(i))
            }
        }
        points
    }
    
    override def initialBasis = {
        for (j <- Array.range(0, 3)) yield {
            for (k <- Array.range(0, 3)) yield InitialBasisMatrix.value(j)(k)
        }
    }
    
    override def isTiling = sides.forall(_.isConstant) && math.abs(sumSideAngles - 2.0 * math.Pi) < Tolerance

    override def transformBasis(basis: Array[Array[Computed]]) {
        for (i <- sides.indices) {
            shiftToNextVertex(basis, computedSides(i))
            if (i < sides.length - 1) {
                rotateSideNormal(basis, computedCosines(i))
            }
        }
    }
    
    override def getObjective(basis: Array[Array[Computed]]) = {
        getLastVertexMismatch(basis(0)) + getLastAngleMismatch(basis(2), computedCosines.last)
    }
    
    override def getMultiplicity = {
        val points = getPoints
        var center = MyVec(3)
        points.foreach(p => center = center + p)
        center = center * (1.0 / center.length)
        var sumAngle = 0.0
        for (i <- points.indices) {
            val angle = center.getOrientedAngle(points(i), points((i + 1) % points.length))
            sumAngle += angle
        }
        sumAngle / (2.0 * math.Pi)
    }
    
    override def getSideValue(side: Double) = math.acos(side)
    
    override def toString = "\nSides: " + ScalaRunTime.stringOf(sides) + "\nCosines: " + ScalaRunTime.stringOf(cosines)
}

object SphericalPolygonEvaluator {
    def apply(polygonElements: PolygonElements, varCount: Int): SphericalPolygonEvaluator = {
        SphericalPolygonEvaluator(polygonElements.sides, polygonElements.cosines, varCount)
    }
}