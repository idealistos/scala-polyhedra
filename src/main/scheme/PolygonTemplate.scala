package main.scheme

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }

case class PolygonTemplate(name: String, angleStrs: Seq[String]) extends PolyScheme {
    // angleStrs: e.g, Vector("a1b1", "b2b2", b1a1") for isosceles triangle or Vector("a1a1", "a2a2") for rhombus
    // "a1b1" means that at vertex 1, there are edges a1-a1 (Edge) and b1-b2 (Side)
    // marker in EdgeType is needed e.g. for "pseudo-trapezoid" a1b1-b2a2-a2b2-b1a1 (edge a2-a2 is "edgeA2")
    
    val map = {
        val eTypeMap = MHashMap[(Int, Int), EdgeType]()
        for (i <- 0 until angleStrs.size) {
            val iNext = (i + 1) % angleStrs.size
            val c1 = angleStrs(i)(2) - 'a'
            val m1 = (angleStrs(i)(3) - '1') / 2
            val d1 = (angleStrs(i)(3) - '1') % 2
            val c2 = angleStrs(iNext)(0) - 'a'
            val m2 = (angleStrs(iNext)(1) - '1') / 2
            val d2 = (angleStrs(iNext)(1) - '1') % 2
            if (c1 != c2 || m1 != m2) {
                throw new Exception("Mismatch")
            }
            val isSide = d1 != d2
            eTypeMap.getOrElseUpdate((c1, m1), EdgeType(c1, isSide, m1))
        }
        eTypeMap
    }

    val list = map.values.toArray.sortBy(_.iEdge)
    
    def minFactor = math.max(4 - angleStrs.size, 1)

    def getRelations(factor: Int) = {
        val relationMap = MHashMap[String, RelationLine]()
        val pCount = getParameterCount(factor)
        var pIndex = 0
        for (angleStr <- angleStrs) {
            val normalized = normalize(angleStr)
            if (!relationMap.contains(normalized)) {
                val c1 = normalized(0) - 'a'
                val c2 = normalized(2) - 'a'
                val m1 = (normalized(1) - '1') / 2
                val m2 = (normalized(3) - '1') / 2
                val d1 = (normalized(1) - '1') % 2
                val d2 = (normalized(3) - '1') % 2
                val eType1 = map((c1, m1))
                val eType2 = map((c2, m2))
                val value = if (pIndex < pCount) {
                    pIndex += 1
                    s"c${name.capitalize}${pIndex}"
                } else {
                    null
                }                
                relationMap(normalized) = RelationLine(
                        eType1.name,
                        eType2.name,
                        eType1.getVertexName(d1),
                        eType2.getVertexName(d2),
                        value)
            }
        }
        relationMap.values.toArray.sortBy(r => (r.fName1, r.fName2))
    }
    
    def normalize(angleStr: String) = {
        val reversed = angleStr.slice(2, 4) + angleStr.slice(0, 2)
        if (angleStr < reversed) angleStr else reversed
    }

    def varAngleCount = {
        val types = MHashSet[String]() ++ (for (angleStr <- angleStrs) yield normalize(angleStr))
        types.size
    }
    
    def constraintCountBySymmetry = {
        // 1 for [b1a1-a2c1] (trapezoid), 2 for [a1-a2b2-b1] (deltoid)
        SymmetricCore.getCoreFirstIndex(this) match {
            case None => 0
            case Some(i) => if (i % 2 == 1 && angleStrs.size % 2 == 0) 1 else 1
        }
    }

    // pa-Aq-qA-ap: *1 -> 0, *2 -> 1 (2 angles)
    // pa-AA-aq: *1 -> 0, *2 -> 1 (2 angles)
    // aB-bC-cA: *1 -> 0, *2 -> 2 (3 angles)
    // aB-bC-cD-dA: *1 -> 1, *2 -> 3 (4 angles)
    // aA-Aa: *n -> 1 (2 angles)
    // pq-qp: *n -> 0 (1 angle)

    def getParameterCount(factor: Int) = {
        if (factor == 1) {
            varAngleCount - 3 + constraintCountBySymmetry
        } else {
            // We can only find a single variable using the fact that the sum of all angles = pi (1 - 2 / n)
            varAngleCount - 1
        }
    }
    
    def toClassCode(factorInfo: FactorInfo) = {
        val n = angleStrs.size * factorInfo.factor
        val lines = ArrayBuffer[String]()
        val parameters = for (i <- 0 until getClassParameterCount(factorInfo)) yield {
            s"c${name.capitalize}${i + 1}: Value"
        }
        lines += s"case class ${name.capitalize}(${parameters.mkString(", ")}) extends Polygon($n) {"
        for (eType <- list) {
            lines += eType.toCode
        }
        for (relation <- getRelations(factorInfo.factor)) {
            lines += relation.toCode
        }
        lines += "}"
        lines.mkString("\n")
    }
    
    def toInstanceCode(factorInfo: FactorInfo) = {
        val parameters = for (i <- 0 until getClassParameterCount(factorInfo)) yield s"x(${i + factorInfo.iNextVar})"
        s"val $name = new ${name.capitalize}(${parameters.mkString(", ")})"
    }
    
    def getClassParameterCount(factorInfo: FactorInfo) = math.min(getParameterCount(factorInfo.factor), factorInfo.extraConstraintCount)
    
    def getOutsideParameters(factorInfo: FactorInfo) = {
        for (i <- Array.range(getClassParameterCount(factorInfo), getParameterCount(factorInfo.factor))) yield {
            s"c${name.capitalize}${i + 1}"
        }
    }
    
}
