package main

import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, HashSet => MHashSet}
import scala.runtime.ScalaRunTime
import weka.core.ConjugateGradientOptimization


case class SphericalPolygon(sides: Array[Value], cosines: Array[Value]) {
    val computedSides: Array[Computed] = for (side <- sides) yield null
    val computedCosines: Array[Computed] = for (cosine <- cosines) yield null

    val varCount = {
        var varCount = 0
        for (side <- sides) {
            varCount = math.max(varCount, side.varCount)
        }
        for (cosine <- cosines) {
            varCount = math.max(varCount, cosine.varCount)
        }
        varCount
    }

    def compute(varArray: Array[Double]) {
        for ((side, i) <- sides.zipWithIndex) {
            computedSides(i) = side.compute(varArray)
        }
        for ((cosine, i) <- cosines.zipWithIndex) {
            computedCosines(i) = cosine.compute(varArray)
        }
    }
    
    def shiftToNextVertex(basis: Array[Array[Computed]], side: Computed) {
        
    }
    
    def rotateSideNormal(basis: Array[Array[Computed]], cosine: Computed) {
        
    }
    
    def transformBasis(basis: Array[Array[Computed]]) {
        for (i <- sides.indices) {
            shiftToNextVertex(basis, computedSides(i))
            if (i < sides.length - 1) {
                rotateSideNormal(basis, computedCosines(i))
            }            
        }
    }
    
    override def toString = "\nSides: " + ScalaRunTime.stringOf(sides) + "\nCosines: " + ScalaRunTime.stringOf(cosines)
}

class SphericalPolygonSolver(sphericalPolygons: IndexedSeq[SphericalPolygon])
        extends ConjugateGradientOptimization {
    
    var cachedVarArray: Array[Double] = null
    var cachedComputed: Computed = null
    var varIndices: Array[Int] = null
    var zeroDFs: Array[Double] = null

    val initialBasis = for (i <- Array.range(0, 3)) yield for (j <- Array.range(0, 3)) yield if (i == j) 1.0 else 0.0
    
    def getLastVertexMismatch(vertex: Array[Computed]) = {
        // |v - (1, 0, 0)|^2 and its derivatives
        var mismatch = 0.0
        val dfs = zeroDFs.clone
        for ((Computed(z, dz), z0) <- vertex zip initialBasis(0)) {
            mismatch += (z - z0) * (z - z0)
            for (i <- varIndices) {
                dfs(i) += 2.0 * (z - z0) * dz(i)
            }
        }
        Computed(mismatch, dfs)
    }
    
    def getLastAngleMismatch(normal: Array[Computed], cosine: Computed) = {
        // (n . (0, 0, 1) - cos(alpha))^2 and its derivatives
        val Computed(c, dc) = cosine
        val Computed(n2, dn2) = normal(2)
        Computed((n2 - c) * (n2 - c), for (i <- varIndices) yield 2.0 * (n2 - c) * (dn2(i) - dc(i)))
    }
    
    def compute(varArray: Array[Double]) {
        varIndices = Array.range(0, varArray.length)
        zeroDFs = for (i <- varIndices) yield 0.0
        var computed = Computed(0.0, zeroDFs)
        for (polygon <- sphericalPolygons) {
            polygon.compute(varArray)
            val basis = for (j <- Array.range(0, 3)) yield {
                for (k <- Array.range(0, 3)) yield Computed(initialBasis(j)(k), zeroDFs)
            }
            polygon.transformBasis(basis)
            computed += getLastVertexMismatch(basis(0))
            computed += getLastAngleMismatch(basis(2), polygon.computedCosines.last)
        }
        cachedVarArray = varArray
    }
    
    override def objectiveFunction(varArray: Array[Double]) = {
        if (varArray != cachedVarArray) {
            compute(varArray)
        }
        cachedComputed.f
    }

    
    override def evaluateGradient(varArray: Array[Double]) = {
        if (varArray != cachedVarArray) {
            compute(varArray)
        }
        cachedComputed.dfs
    }
    
    override def getRevision = ""
    
}

object SphericalPolygonSolver {

    def solve(sphericalPolygons: IndexedSeq[SphericalPolygon]) = {
        var varCount = 0
        for (polygon <- sphericalPolygons) {
            varCount = math.max(varCount, polygon.varCount)
        }
        val solver = new SphericalPolygonSolver(sphericalPolygons)
        var varArray = Array.fill(varCount)(0.5)
        val constraints = for (i <- Array.range(0, 2)) yield Array.fill(varCount)(Double.NaN)
        varArray = solver.findArgmin(varArray, constraints)
        var iStep = 0
        while (varArray == null && iStep <= 100) {
            varArray = solver.getVarbValues()
            varArray = solver.findArgmin(varArray, null)
            iStep += 1
            print("Iteration " + iStep + ": " + solver.getMinFunction())
        }
        varArray
    }  
    
}