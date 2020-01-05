package main.poly

import main.value.Computed
import main.value.Value

abstract class PolygonEvaluator {
    def varCount: Int
    def sides: Array[Value]
    def cosines: Array[Value]
    val zeroDFs = Array.fill(varCount)(0.0)

    val computedSides: Array[Computed] = for (side <- sides) yield null
    val computedCosines: Array[Computed] = for (cosine <- cosines) yield null
    
    def initialBasis: Array[Array[Double]]

    def computedInitialBasis = {
        val basis = initialBasis
        for (j <- basis.indices.toArray) yield {
            for (k <- basis(0).indices.toArray) yield Computed(basis(j)(k), zeroDFs)
        }
    }

    def compute(varArray: Array[Double]) {
        for ((side, i) <- sides.zipWithIndex) {
            computedSides(i) = side.compute(varArray)
        }
        for ((cosine, i) <- cosines.zipWithIndex) {
            computedCosines(i) = cosine.compute(varArray)
        }
    }
    
    def isTiling = false
    
    def transformBasis(basis: Array[Array[Computed]]): Unit
    def getObjective(basis: Array[Array[Computed]]): Computed
    def getMultiplicity: Double
    def getSideValue(side: Double): Double // Used for the triangle inequality
   
    def getLastVertexMismatch(vertex: Array[Computed]) = {
        // |v - (1, 0, 0)| and its derivatives
        var distance = 0.0
        for ((Computed(z, _), z0) <- vertex zip initialBasis(0)) {
            distance += (z - z0) * (z - z0)
        }
        val dfs = zeroDFs.clone
        for ((Computed(z, dz), z0) <- vertex zip initialBasis(0)) {
            for (i <- 0 until varCount) {
                dfs(i) += 2.0 * (z - z0) * dz(i)
            }
        }
        Computed(distance, dfs)
    }
    
    def isValid = {
        if (!sides.forall(_.isConstant)) {
            true // We'll find out if it is valid during the optimization
        } else {
            val values = sides.map(x => getSideValue(x.constantValue))
            values.sum > 2.0 * values.max
        }
    }
    
}