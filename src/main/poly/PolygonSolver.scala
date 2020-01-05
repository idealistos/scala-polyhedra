package main.poly

import scala.util.Random
import java.util.Arrays
import weka.core.Optimization
import main.value.Computed


case class PolygonSolver(evaluators: IndexedSeq[PolygonEvaluator])
        extends Optimization {
    
    val varCount = evaluators.foldLeft(0)((count, polygon) => math.max(count, polygon.varCount))
    val varRange = Array.range(0, varCount)
    val zeroDFs = evaluators(0).zeroDFs

    var cachedVarArray: Array[Double] = null
    var cachedComputed: Computed = null
    var factor = 1.0 // Needed to increase the precision of the solution
    val random = new Random(0)
    
    def compute(varArray: Array[Double]) {
        var computed = Computed(0.0, zeroDFs)
        for (evaluator <- evaluators) {
            evaluator.compute(varArray)
            val basis = evaluator.computedInitialBasis
            evaluator.transformBasis(basis)
            computed += evaluator.getObjective(basis)
        }
        cachedVarArray = varArray.clone
        cachedComputed = computed
        // println(s"Objective/gradient: $computed for ${varArray.mkString(", ")}")
    }
    
    def getMultiplicity(varArray: Array[Double]) = {
        var maxMultiplicity = 0.0
        for (evaluator <- evaluators) {
            evaluator.compute(varArray)
            maxMultiplicity = math.max(maxMultiplicity, evaluator.getMultiplicity)
        }
        maxMultiplicity
    }
    
    override def objectiveFunction(varArray: Array[Double]) = {
        if (!Arrays.equals(varArray, cachedVarArray)) {
            compute(varArray)
        }
        cachedComputed.f * factor
    }
    
    override def evaluateGradient(varArray: Array[Double]) = {
        if (!Arrays.equals(varArray, cachedVarArray)) {
            compute(varArray)
        }
        for (df <- cachedComputed.dfs) yield factor * df
    }
    
    override def getRevision = ""

    def iterate(varArray: Array[Double], constraints: Array[Array[Double]], tolerance: Double) = {
        Optimization.m_Zero = tolerance
        Optimization.m_Epsilon = math.pow(tolerance, 2.0)
        m_TOLX = 100.0 * tolerance
        var x = findArgmin(varArray, constraints)
        var iStep = 0
        while (x == null && iStep <= 100) {
            x = getVarbValues()
            x = findArgmin(x, null)
            iStep += 1
        }
        println(s"Found solution for factor = $factor: ${x.mkString(", ")}; objective: ${getMinFunction() / factor}")
        x
    }
    
    def randomInitialVars = varRange.map(_ => 0.99 * (2.0 * random.nextDouble - 1.0))
    def initialConstraints = for (bound <- Array(-0.999, 0.999)) yield Array.fill(varCount)(bound)
    
    def getInexactSolution = {
        var solution: Array[Double] = null
        factor = 1.0
        var iTry = 0
        while (solution == null && iTry < 50) {
            try {
                val varArray = iterate(randomInitialVars, initialConstraints, 1e-8)
                if (math.abs(objectiveFunction(varArray)) < 1e-8) {
                    val multiplicity = getMultiplicity(varArray)
                    if (math.abs(multiplicity - 1.0) < 1e-8) {
                        solution = varArray
                    } else {
                        println(s"Solution rejected because of multiplicity $multiplicity")
                    }
                }
            } catch {
                case e: Exception => e.printStackTrace()
                // case e: Exception => println(s"Error: $e")
            }
            iTry += 1
        }
        solution
    }
    
    def improveSolution(varArray: Array[Double]) = {
        factor = 1e8
        val bound1 = for (x <- varArray) yield x - 1e-3
        val bound2 = for (x <- varArray) yield x + 1e-3
        iterate(varArray, Array(bound1, bound2), 1e-20)
    }
    
    def solve = {
        if (evaluators.forall(_.isTiling)) {
            Array.fill(varCount)(-1.0)
        } else {
            var varArray = getInexactSolution
            if (varArray == null || math.abs(objectiveFunction(varArray)) > 1e-8) {
                throw new Exception("Cannot find a solution!")
            }
            
            varArray = improveSolution(varArray)
            
            if (math.abs(objectiveFunction(varArray) / factor) > 1e-10) {
                throw new Exception("Cannot find a solution!")
            }
            varArray
        }
    }  
 
}

