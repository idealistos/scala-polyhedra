package main.scheme

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import main.value.Implicits._

class EulerChecker(faceTypes: Array[Char], faceTemplates: Array[PolygonTemplate], sameFaceTypes: Array[Int]) {
    // faceTypes: iAngle -> face type ('A', 'B', etc.)
    // sameFaceTypes: e.g., if 'A' and 'C' are interchangeable, sameFaceTypes(2) will be 0; default: -1
    
    
    val faceTypeCount = faceTemplates.length
    val angleCount = faceTypes.length
    
    val inverseFactors = {
        // Array q_i for the condition q_0 / factor(face_0) + .. + q_{n-1} / factor(face_{n-1}) + q_n / factor(this) >= 1
        // Source: (V count) * (1 - f (Sum (E_i in V) / 2) + f (Sum (F_i in V) / (factor(F_i) * (V of F_i)))) = 2
        // Therefore, q_n = 2 / (E in V), q_i = q_n (F_i in V) / (V of F_i)
        val eFactor = 2.0 / angleCount
        val factors = ArrayBuffer[Double]()
        val faceInVertexCounts = faceTypes.groupBy(x => x).mapValues(_.size)
        factors ++= (for (iFace <- 0 until faceTypeCount) yield {
            eFactor * faceInVertexCounts(('A' + iFace).toChar) / (faceTemplates(iFace).angleStrs.length)
        })
        factors += eFactor
        factors.toArray
    }
    
    val minFactors = {
        val factors = ArrayBuffer[Int]()
        factors ++= (for (faceTemplate <- faceTemplates) yield faceTemplate.minFactor)
        factors += math.max(4 - angleCount, 1) // Should be minFactor * angleCount >= 3
        factors.toArray
    }

    // factorOrder(0) is the index of max in inverseFactors
    val factorOrder = inverseFactors.zipWithIndex.sortBy(-_._1).map(_._2)
    
    def checkSameFaceTypes(factors: Array[Int]) = {
        // Ensures that, if, e.g., 'C' is same as 'A', factors('C') <= factors('A')
        factors.zipWithIndex forall {
            _ match {
                case (f, i) if i < faceTemplates.length => sameFaceTypes(i) == -1 || factors(sameFaceTypes(i)) >= f
                case _ => true
            }
        }
    }
    
    def addFactorLists(index: Int, factors: Array[Int], currentSum: Double, factorLists: ArrayBuffer[Array[Int]]) {
        if (index == factors.length) {
            if (checkSameFaceTypes(factors)) {
                factorLists += factors.clone
            }
        } else {
            val i = factorOrder(index)
            val factor0 = factors(i)
            for (factor <- factor0 to 8) {
                val newSum = currentSum - inverseFactors(i) * (1.0 / factors(i) - 1.0 / factor)
                if (newSum > 1.0 - Tolerance) {
                    factors(i) = factor
                    addFactorLists(index + 1, factors, newSum, factorLists)
                    factors(i) = factor0
                }
            }
        }
    }
    
    def getValidFactorMaps = {
        val factors = minFactors.clone
        val factorLists = ArrayBuffer[Array[Int]]()
        val currentSum = (factors zip inverseFactors).foldLeft(0.0)((v, pair) => v + pair._2 / pair._1)
        addFactorLists(0, factors, currentSum, factorLists)
        
        for (factorList <- factorLists.toArray) yield {
            val factorMap = for ((faceTemplate, i) <- faceTemplates.zipWithIndex) yield {
                faceTemplate.name -> factorList(i)
            }
            HashMap[String, Int]("this" -> factorList(faceTypeCount)) ++ factorMap
        }
    }
    
    def isTiling(factorMap: Map[String, Int]) = {
        val products = for ((faceTemplate, i) <- faceTemplates.zipWithIndex) yield {
            inverseFactors(i) / factorMap(faceTemplate.name)
        }
        val total = products.sum + inverseFactors(faceTypeCount) / factorMap("this") 
        math.abs(total - 1.0) < Tolerance
    }    
    
}