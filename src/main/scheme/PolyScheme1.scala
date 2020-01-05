package main.scheme
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import scala.collection.mutable.ArrayBuffer


case class PolyScheme1(angleStrs: Seq[String]) extends PolyScheme {
    // E.g., for snub cube: a1a2, a2c1, c2b1, b1b2, b2a1
    
    def countByEdgeType = {
        val counts1 = Array.fill[Int](edgeTypeCount)(0)
        for (s <- angleStrs) {
            counts1(s(0) - 'a') += 1
        }
        counts1
    }
    
    def onesPrevalence(iEdge: Int) = {
        // E.g., for iEdge = 0, counts numbers of "a1" and "a2" and returns #a1 - #a2
        var n1 = 0
        var n2 = 0
        for (s <- angleStrs; i <- 0 to 1) {
            if (s(2 * i) == 'a' + iEdge) {
                if (s(2 * i + 1) == '1') {
                    n1 += 1
                } else {
                    n2 += 1
                }
            }
        }
        n1 - n2
    }
    
    def hasEdge(angleStr: String, iEdge: Int) = angleStr(0) == 'a' + iEdge || angleStr(2) == 'a' + iEdge
    
    def shifted(iSteps: Int) = {
        // E.g., from "a1a2, a2c1", shifted(0) = "a1", shifted(1) = "a2", etc.
        val s = angleStrs(iSteps / 2)
        s.slice((iSteps % 2) * 2, (iSteps % 2) * 2 + 2)
    }
    
    def isSymmetric(pos: Int) = {
        // E.g., a1a1, a1b1, b1a1 is symmetric with pos = 0; a1a1, a1b1, b1c1, c1a1 is not
        val n = angleStrs.length
        var symmetric = true
        for (i <- 0 until n) {
            val s1 = shifted((pos * 2 + i + 1) % (2 * n))
            val s2 = shifted((pos * 2 - i + 2 * n) % (2 * n))
            symmetric &= (s1 == s2)
        }
        symmetric
    }
    
    def hasValidTypeVs = {
        // Each edge is used and either symmetric (with marker = 1) or asymmetric
        edgeTypeVs.forall(_ match {
            case 0 => false
            case 2 => false
            case _ => true
        })        
    }
    
    def hasValidMatchTypes = {
        // Each edge either "matches" or "opposes", not both
        edgeMatchTypes.forall(_ match {
            case 0 => false
            case 3 => false
            case _ => true
        })        
    }
    
    def countsByEdgeTypeIncrease = {
        val counts = countByEdgeType
        (1 until edgeTypeCount).forall(i => counts(i - 1) >= counts(i))
    }
    
    def sameInAndOutCounts = {
        // E.g., returns false for a1a1-a1a2-a2a1 because has 4 "a1" but only two "a2"
        val ins = Array.fill(edgeTypeCount)(0)
        val outs = Array.fill(edgeTypeCount)(0)
        for (s <- angleStrs; i <- 0 to 1) {
            val edgeType = s(i * 2) - 'a'
            if (edgeTypeVs(edgeType) == 3) {
                if (s(i * 2 + 1) == '1') {
                    ins(edgeType) += 1
                } else {
                    outs(edgeType) += 1
                }
            }
        }
        (ins zip outs).find(pair => pair._1 != pair._2) match {
            case None => true
            case _ => false
        }
    }
    
    def isValid: Boolean = {
        for (i <- angleStrs.indices) {
            val iNext = (i + 1) % angleStrs.length
            val c1 = angleStrs(i)(2)
            val c2 = angleStrs(iNext)(0)
            if (c1 != c2) {
                return false
            }
            val m1 = angleStrs(i)(3) - '0'
            val m2 = angleStrs(iNext)(1) - '0'
            if (m1 != m2 && m1 + m2 != 3) {
                return false
            }
            if (c1 == angleStrs(i)(0) && m1 == (angleStrs(i)(1) - '0') && !isSymmetric(i)) {
                return false
            }
        }
        val moreOnes = (0 until edgeTypeCount).forall(onesPrevalence(_) >= 0)
        hasValidTypeVs && hasValidMatchTypes && countsByEdgeTypeIncrease && moreOnes && sameInAndOutCounts
    }
    
    def toCharArray = (for {
        s <- angleStrs
        i <- 0 until 4
    } yield s(i)).toArray 
    
    def getSwapLetterReorderingRules = {
        val rules = ArrayBuffer[ReorderingRule]()
        val counts = countByEdgeType
        for (i <- 1 to angleStrs.length) { // counts.zipWithIndex = (count, letter - 'a') for letter = 'a',..,
            val withCount = counts.zipWithIndex.filter(_._1 == i).map(_._2)
            if (withCount.length > 1) {
                for (j <- 1 until withCount.length) {
                    rules += SwapLetterReorderingRule(('a' + withCount(j - 1)).toChar, ('a' + withCount(j)).toChar)
                }
            }
        }
        rules
    }
    
    def getSwapMarkerReorderingRules = {
        val rules = ArrayBuffer[ReorderingRule]()
        for (et <- 0 until edgeTypeCount) {
            if (onesPrevalence(et) == 0) {
                val ePositions = for {
                    i <- angleStrs.indices
                    j <- 0 to 3
                    if angleStrs(i)(j) == 'a' + et
                } yield (i * 4 + j)
                rules += SwapMarkerReorderingRule(ePositions(0))
            }
        }
        rules
    }
    
    def getReorderingRules = getSwapLetterReorderingRules ++ getSwapMarkerReorderingRules
    
    def checkReorderings(visited: MHashSet[String]) = {
        // Replaces a <-> b, a1 <-> a2, etc. as well as a1 <-> a2, etc., and checks that the result is not in visited
        val rules = getReorderingRules
        var found = false
        var index = 0
        val queue = ArrayBuffer[Array[Char]]()
        val visitedNodes = MHashSet[String]()
        queue += toCharArray
        while (!found && index < queue.length) {
            val chars = queue(index)
            for (rule <- rules) {
                rule.applyTo(chars)
                val charsStr = chars.mkString("")
                if (!visitedNodes.contains(charsStr)) {
                    if (visited.contains(charsStr)) {
                        found = true
                    } else {
                        queue += chars.clone
                        visitedNodes.add(charsStr)
                    }
                }
                rule.applyTo(chars)
            }
            index += 1
        }
        !found        
    }
    
}


object PolyScheme1 {

    def rearrange(chars: Array[Char]) = {
        for (i <- Array.range(0, chars.length / 4)) yield chars.subSequence(i * 4, (i + 1) * 4).toString
    }
    
    def mirror(strTypes: Array[String]) {
        def reverse(s: String) = s.slice(2, 4) + s.slice(0, 2)
        val n = strTypes.length
        for (i <- 0 until n / 2) {
            val v1 = reverse(strTypes(i))
            strTypes(i) = reverse(strTypes(n - i - 1))
            strTypes(n - i - 1) = v1
        }
        if (n % 2 == 1) {
            strTypes(n / 2) = reverse(strTypes(n / 2))
        }
    }

    def checkStrTypes(strTypes: Array[String], visited: MHashSet[String]) = {
        var normalForm: Array[String] = null
        var normalFormStr: String = null
        val n = strTypes.length
        for (k <- 0 to 1) {
            for (i <- 0 until n) {
                val v0 = strTypes(0)
                for (j <- 1 until n) {
                    strTypes(j - 1) = strTypes(j)
                }
                strTypes(n - 1) = v0
                val str = strTypes.mkString("")
                if (!visited.contains(str)) {
                    val copy = strTypes.clone
                    visited.add(str)
                    import scala.math.Ordering.Implicits._
                    if (normalForm == null || str < normalFormStr) {
                         normalFormStr = str
                         normalForm = copy
                    }
                }
            }
            mirror(strTypes)
        }
        normalForm match {
            case null => None
            case x => Some(x)
        }
    }
    
    def getValidChars(soFar: Array[Char], i: Int, edgeTypeCount: Int) = {
        if (i % 4 == 0 || i % 4 == 2) {
            if (i % 4 == 2 || i == 0) {
                for (et <- Array.range(0, edgeTypeCount)) yield ('a' + et).toChar
            } else {
                Array(soFar(i - 2))
            }
        } else {
            Array('1', '2')
        }
    }
    
    def addVariants(polySchemes1: ArrayBuffer[PolyScheme1], visited: MHashSet[String], soFar: Array[Char], i: Int, edgeTypeCount: Int) {
        // Generates letters and digits, e.g., 'a', '1', 'b', '1',...
        val angleCount = soFar.length / 4
        if (i == angleCount * 4) {
            val strTypes = rearrange(soFar)
            val polyScheme1 = PolyScheme1(strTypes)
            if (polyScheme1.isValid && polyScheme1.edgeTypeCount == edgeTypeCount
                    && polyScheme1.checkReorderings(visited)) {
                val normalForm = checkStrTypes(strTypes, visited)
                normalForm match {
                    case Some(strTypes1) => polySchemes1 += PolyScheme1(strTypes1)
                    case None =>
                }
            }
        } else {
            val validChars = getValidChars(soFar, i, edgeTypeCount)
            for (validChar <- validChars) {
                soFar(i) = validChar
                addVariants(polySchemes1, visited, soFar, i + 1, edgeTypeCount)
            }
        }        
    }
    
    def explore(angleCount: Int, edgeTypeCount: Int) = {
        val polySchemes1 = ArrayBuffer[PolyScheme1]()
        val visited = MHashSet[String]()
        val soFar = Array.fill[Char](angleCount * 4)('-')
        addVariants(polySchemes1, visited, soFar, 0, edgeTypeCount)
        polySchemes1.toArray        
    }
    
}