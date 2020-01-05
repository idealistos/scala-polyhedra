package main.scheme
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet, Map => MMap, Set => MSet }
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import main.value.Implicits._

case class PolyScheme3(angleStrs: Seq[String]) extends PolyScheme {
   
    val hashes = (for (s <- angleStrs; j <- 0 to 1) yield SymmetricCore.toHash(s.slice(2 * j, 2 * j + 2))).toArray
    val faceTypes = Array.fill[Char](angleStrs.length)('-')
    val polyAngleStrsList = ArrayBuffer[Array[String]]()
    
    
    def typeCountStr = {
        val faceTypeCount = polyAngleStrsList.size
        val faceWord = if (faceTypeCount == 1) "Face" else "Faces"
        val edgeWord = if (edgeTypeCount == 1) "Edge" else "Edges"
        s"$faceTypeCount$faceWord${edgeTypeCount}$edgeWord"
    }
    
    def polyIsClosed(faceHashes: Seq[Int]) = {
        val hash = faceHashes.last
        val hash0 = faceHashes(0)
        (hash / 2) == (hash0 / 2) && ((hash % 2) == (hash0 % 2)) == (edgeTypeVs(hash / 4) == 1)
    }
    
    def mirrorAngle(angleStr: String) = angleStr.slice(2, 4) + angleStr.slice(0, 2)
    
    def getOpposingAngles(iEdge: Int, marker: Char, faceRestriction: Int) = {
        angleStrs.zipWithIndex.collect {
            case (s, i) if s(2) == 'a' + iEdge && s(3) != marker
                    && (faceRestriction < 0 || faceTypes(i) == '*' || faceTypes(i) == faceRestriction) => (mirrorAngle(s), i)
            case (s, i) if s(0) == 'a' + iEdge && s(1) != marker
                    && (faceRestriction < 0 || faceTypes(i) == '*' || faceTypes(i) == faceRestriction)=> (s, i)
        }
    }
    
    def getFaceType(index: Int) = faceTypes(index / 2)
    
    def getNextIndex(nextHash: Int, faceType: Char) = {
        val indices = lookupMap(nextHash)
        val faceType1 = getFaceType(indices(0))
        if (indices(1) == -1) {
            assert(faceType1 == faceType || faceType1 == '-')
            indices(0)            
        } else {
            val faceType2 = getFaceType(indices(1))
            if (faceType1 == faceType) {
                assert(faceType2 == '-')
                indices(1)
            } else {
                if (faceType1 != '-') {
                    println("Problem!")
                }
                assert(faceType1 == '-')
                indices(0)
            }
        }
    }
    
    def growPoly(faceType: Char, faceHashes: ArrayBuffer[Int]) {
        val nextHash = getNextHash(faceHashes.last)
        val nextIndex = getNextIndex(nextHash, faceType)
        faceTypes(nextIndex / 2) = faceType
        faceHashes += nextHash
        faceHashes += hashes(2 * (nextIndex / 2) + (1 - nextIndex % 2)) 
    }
    
    def markSameAngles(faceType: Char) {
        // E.g., with a1b1(A)-b1a1(-)-a2b2(A)-b2a2(-), marks b1a1 and b2a2 as A
        val toSet = MHashSet[String]()
        for ((s, faceType1) <- angleStrs zip faceTypes if faceType1 == faceType) {
            toSet += s
            toSet += mirrorAngle(s)
        }
        for ((s, i) <- angleStrs.zipWithIndex) {
            if (faceTypes(i) == '-' && toSet.contains(s)) {
                faceTypes(i) = faceType
            }
        }
    }
    
    def findNextFace() = {
         faceTypes.indexOf('-') match {
             case -1 => false
             case iAngle => {
                 val faceHashes = ArrayBuffer[Int](hashes(2 * iAngle), hashes(2 * iAngle + 1))
                 val faceType = ('A' + polyAngleStrsList.size).toChar
                 faceTypes(iAngle) = faceType
                 while (!polyIsClosed(faceHashes)) {
                     growPoly(faceType, faceHashes)
                 }
                 markSameAngles(faceType)
                 val faceEdges = faceHashes.map(SymmetricCore.fromHash)
                 polyAngleStrsList += (for (i <- Array.range(0, faceEdges.length / 2)) yield faceEdges(i * 2) + faceEdges(i * 2 + 1))
                 true
             }
         }
    }
    
    lazy val lookupMap = {
        // lookupMap: hash of an edge (e.g., a1) -> one or two positions in hashes
        val lookupMap = Array.tabulate[Int](edgeTypeCount * 4, 2)((x1, x2) => -1)
        for ((hash, i) <- hashes.zipWithIndex) {
            if (lookupMap(hash)(0) != -1) {
                lookupMap(hash)(1) = i
            } else {
                lookupMap(hash)(0) = i
            }
        }
        lookupMap
    }
    
    def getNextHash(hash: Int) = {
        // E.g., a1 -> a1 (for symmetric edges), a1 -> a2 or b4 -> b3 (for asymmetric edges)
        if (edgeTypeVs(hash / 4) == 1) {
            hash
        } else {
            2 * (hash / 2) + (1 - hash % 2)
        }
    }
    
    while (findNextFace()) { }
    
    def isValid = {
        // Checks that every edge is shared by the same 2 faces - e.g., prevents A(a1a1)-B(a1a1)-C(a1a1)
        val faceTypesByET = Array.fill(edgeTypeCount)(Array.fill(2)('-'))
        var mismatchFound = false
        for ((faceType, i) <- faceTypes.zipWithIndex) {
            if (!mismatchFound) {
                val eType = angleStrs(i)(2) - 'a'
                if (faceTypesByET(eType)(0) == '-') {
                    faceTypesByET(eType)(0) = faceType
                } else if (faceTypesByET(eType)(0) != faceType) {
                    val oldFaceType = faceTypesByET(eType)(1)
                    if (oldFaceType != '-' && oldFaceType != faceType) {
                        mismatchFound = true
                    } else {
                        faceTypesByET(eType)(1) = faceType
                    }
                }
            }
        }
        !mismatchFound
    }
    
    val faceTemplates = Array.range(0, polyAngleStrsList.size).map {
        i => PolygonTemplate(s"face${('A' + i).toChar}", polyAngleStrsList(i))
    }
    
    def tryShift(shift: Int, direction: Int, links: MSet[(Int, Int)]) {
        // E.g., for angleStrs = a1b1, b1c1, c1a1 and shift = 1, tries to match b1c1, c1a1, a1b1 (+ faces)
        // With direction = -1 would be mirrored, then shifted (b1a1, a1c1, c1b1 -> a1c1, c1b1, b1a1)
        val n = faceTypes.length
        var mismatchFound = false
        val map = MHashMap[String, String]().withDefaultValue(null)
        
        def check(s: String, s0: String) {
            val s1 = map(s0)
            if (s1 == null || s1 == s) {
                map(s0) = s
            } else {
                mismatchFound = true
            }
        }
        
        for (i <- 0 until n) {
            if (!mismatchFound) {
                val shiftedFace = faceTypes((shift + i * direction + n) % n)
                val angle = angleStrs((shift + i * direction + n) % n)
                val shiftedAngle = if (direction == -1) angle.slice(2, 4) + angle.slice(0, 2) else angle
                check(faceTypes(i).toString, shiftedFace.toString)
                check(angleStrs(i).slice(0, 2), shiftedAngle.slice(0, 2))
                check(angleStrs(i).slice(2, 4), shiftedAngle.slice(2, 4))
            }            
        }
        if (!mismatchFound) {
            for ((sFrom, sTo) <- map) {
                if (sFrom.length == 1 && sFrom != sTo) {
                    links.add(sFrom(0) - 'A', sTo(0) - 'A')
                }
            }
        }
    }
    
    def getConnectedComponent(i0: Int, visited: Array[Boolean], adjacent: Array[ArrayBuffer[Int]]) = {
        val queue = ArrayBuffer[Int](i0)
        var index = 0
        visited(i0) = true
        while (index < queue.size) {
            val i = queue(index)
            for (iAdjacent <- adjacent(i) if !visited(iAdjacent)) {
                visited(iAdjacent) = true
                queue += iAdjacent
            }
            index += 1                    
        }
        queue.toArray
    }
    
    def getSameFaceTypes = {
        // E.g., if faces 'A', 'B', and 'C' are all the same, should be sameFaceTypes(2) = 1, sameFaceTypes(1) = 0
        val links = MHashSet[(Int, Int)]()
        for (direction <- Array(-1, 1)) {
            for (shift <- 0 until faceTypes.length) {
                tryShift(shift, direction, links)
            }
        }
        val adjacent = Array.fill(faceTypes.length)(ArrayBuffer[Int]())
        for ((i1, i2) <- links) {
            adjacent(i1) += i2
        }
        val visited = Array.fill(faceTypes.length)(false)
        val sameFaceTypes = Array.fill(faceTypes.length)(-1)
        for (i <- 0 until faceTypes.length) {
            if (!visited(i) && adjacent(i).size > 0) {
                val component = getConnectedComponent(i, visited, adjacent).sorted
                for (i1 <- 1 until component.length) {
                    sameFaceTypes(component(i1)) = component(i1 - 1)
                }
            }
        }
        sameFaceTypes
    }
    
    lazy val eulerChecker = new EulerChecker(faceTypes, faceTemplates, getSameFaceTypes)
    
    def getExtraConstraintCounts = {
        // Finds cases such as A(a1a2)B(a1b1)B(b3a2): there are two connections A|B, one with a1 in B, another one with a2 in B,
        // and this results in an additional constraint on B face (unless this is a tiling)
        val nbToEdges = Array.fill(faceTemplates.length)(Array.fill(faceTemplates.length)(Array.fill[(Int, String)](edgeTypeCount)(null)))
        val eccArray = Array.fill(faceTemplates.length)(0)
        for (i <- 0 until angleStrs.length) {
            for (j <- Array(-1, 1)) {
                val iNext = (i + j + angleStrs.length) % angleStrs.length
                val faceType = faceTypes(i) - 'A'
                val faceType2 = faceTypes(iNext) - 'A'
                val angle = angleStrs(i).slice(j + 1, j + 3)
                val angle2 = angleStrs(iNext).slice(1 - j, 3 - j)
                val iEdge = angle(0) - 'a'
                nbToEdges(faceType)(faceType2)(iEdge) match {
                    case null => nbToEdges(faceType)(faceType2)(iEdge) = i -> angle
                    case (iSaved, angleSaved) if iSaved == i || angleSaved == angle || faceType == faceType2 =>
                    case (iSaved, angleSaved) => eccArray(faceType) += 1
                }
            }
        }
        eccArray
    }
    
    lazy val facePairs = {
        // facePairs[i] is a pair of tuples (face index, "marker index" (e.g., 0 for a1 and a2, 1 for a3 and a4))
        val pairs = Array.tabulate[(Int, Int)](edgeTypeCount, 2)((x1, x2) => (-1, 0))
        for ((faceType, i) <- faceTypes.zipWithIndex) {
            for (j <- 0 to 1) {
                val eType = angleStrs(i)(2 * j) - 'a'
                val indexInFace = (angleStrs(i)(2 * j + 1) - '1') / 2
                if (pairs(eType)(0)._1 == -1) {
                    pairs(eType)(0) = (faceType - 'A', indexInFace)
                } else if (pairs(eType)(0)._1 != faceType - 'A' || pairs(eType)(0)._2 != indexInFace) {
                    pairs(eType)(1) = (faceType - 'A', indexInFace)
                }
            }
        }
        for (pair <- pairs) {
            if (pair(1)._1 == -1) {
                pair(1) = pair(0)
            }
        }
        pairs
    }
    
    def getEdgeRelation(iEdge: Int) = {
        val Array((iFace1, indexInFace1), (iFace2, indexInFace2)) = facePairs(iEdge)
        val faceName1 = faceTemplates(iFace1).name
        val faceName2 = faceTemplates(iFace2).name
        val matchType = if (edgeMatchTypes(iEdge) == 1) "matches" else "opposes"
        val edgeType = if (edgeTypeVs(iEdge) == 1) "edge" else "side"
        val marker1 = if (indexInFace1 == 0) "" else "" + ('0' + indexInFace1).toChar
        val marker2 = if (indexInFace2 == 0) "" else "" + ('0' + indexInFace2).toChar
        val edgeName1 = s"$edgeType${('A' + iEdge).toChar}$marker1"
        val edgeName2 = s"$edgeType${('A' + iEdge).toChar}$marker2"
        s"$faceName1.$edgeName1 $matchType $faceName2.$edgeName2"
    }
    
    def getSomeVertexName = {
        val Array((iFace1, _), _) = facePairs(0)
        val faceName1 = faceTemplates(iFace1).name
        val edgeType = if (edgeTypeVs(0) == 1) "edge" else "side"
        val edgeName = s"${edgeType}A"
        val vertexName = if (edgeType == "edge") "vertex" else "vertex1"
        s"$faceName1.$edgeName.$vertexName"
    }
    
    def getEdgeCategory(iEdge: Int) = {
        val Array((iFace1, _), (iFace2, _)) = facePairs(iEdge)
        if (edgeTypeVs(iEdge) == 1) {
            if (iFace1 == iFace2) 'A' else 'B'
        } else if (edgeMatchTypes(iEdge) == 1) {
            if (iFace1 == iFace2) 'E' else 'F'
        } else {
            if (iFace1 == iFace2) 'C' else 'D'
        }
    }
    
    def getAbbreviation(factorMap: Map[String, Int]) = {
        val edgesAbbreviation = (for (iEdge <- 0 until edgeTypeCount) yield getEdgeCategory(iEdge)).sorted.mkString("")
        val factors = for (faceTemplate <- faceTemplates) yield {
            val factor = factorMap(faceTemplate.name)
            if (factor < 10) s"$factor" else s"_${factor}_"
        }
        val thisFactor = if (factorMap("this") == 1) "" else s"X${factorMap("this")}"
        s"$edgesAbbreviation${factors.mkString("")}${thisFactor}"
    }
    
    def getSideParameters = {
        for (i <- Array.range(0, edgeTypeCount)) yield s"d${('A' + i).toChar}"
    }
    
    def getCosineParameters(factorMap: Map[String, Int], eccArray: Array[Int]) = {
        for {
            (faceTemplate, i) <- faceTemplates.zipWithIndex
            cParameter <- faceTemplate.getOutsideParameters(FactorInfo(factorMap(faceTemplate.name), eccArray(i), 0))
        } yield s"$cParameter"
    }

    def getParameters(factorMap: Map[String, Int], eccArray: Array[Int]) = getSideParameters ++ getCosineParameters(factorMap, eccArray)
    
    def getDescription = {
        val parts = for ((angleStr, faceType) <- angleStrs zip faceTypes) yield s"$faceType($angleStr)"
        parts.mkString("")
    }
    
    def toClassCode(className: String, factorMap: Map[String, Int], eccArray: Array[Int]) = {
        val parameters = getParameters(factorMap, eccArray)
        val parametersStr = parameters.map(_ + ": Value").mkString(", ")
        val header = s"case class $className($parametersStr) extends Poly() {"
        val comment = "// " + getDescription
        val faceClasses = for ((faceTemplate, i) <- faceTemplates.zipWithIndex) yield {
            val factorInfo = FactorInfo(factorMap(faceTemplate.name), eccArray(i), 0)
            faceTemplate.toClassCode(factorInfo)
        }
        var iNextVar = 0
        val faceInstances = for ((faceTemplate, i) <- faceTemplates.zipWithIndex) yield {
            val factorInfo = FactorInfo(factorMap(faceTemplate.name), eccArray(i), iNextVar)
            iNextVar += faceTemplate.getClassParameterCount(factorInfo)
            faceTemplate.toInstanceCode(factorInfo)
        }
        val faceRelations = for (i <- Array.range(0, edgeTypeCount)) yield getEdgeRelation(i)
        val edgeInVertexCount = angleStrs.length * factorMap("this")
        val countByAxis = s"($edgeInVertexCount faces) at ${getSomeVertexName}"
        val lines = Array(header, comment) ++ faceClasses ++ faceInstances ++ faceRelations ++ Array(countByAxis) ++ Array("}")
        lines.mkString("\n")
    }
    
    def toWrapperCode(className: String, factorMap: Map[String, Int], eccArray: Array[Int]) = {
        val sideParameterCount = getSideParameters.length
        val cosineParameterCount = getCosineParameters(factorMap, eccArray).length
        val arguments = for (i <- 0 until sideParameterCount + cosineParameterCount) yield s"v($i)"
        val argumentsStr = arguments.mkString(", ")
        s"PolyWrapper(${'"'}$className${'"'}, $sideParameterCount, $cosineParameterCount, v => $className($argumentsStr))"
    }
    
    override def toString = (angleStrs zip faceTypes).map(x => s"${x._1}(${x._2})").mkString(", ")
}
