package main.scheme

import scala.collection.mutable.ArrayBuffer


case class SymmetricCore(eList: Array[String], startIndex: Int, totalLength: Int, fullEdgeTypeCount: Int) extends PolyScheme {
    
    def isValid(counts: Array[Int], hashes: Seq[Int]) = {
        // Checks that, e.g., in case there are a3/a4 present, only xxa1-a3yy and xxa2-a4yy are present
        // (otherwise the edge "a" is actually a combination of different edges)
        var valid = true
        val adjustedHashes = if (eList.size == totalLength) {
            hashes.last +: hashes :+ hashes(0)
        } else {
            hashes(0) +: hashes :+ hashes.last
        }
        for (i <- adjustedHashes.indices) {
            if (i > 0 && (i + startIndex) % 2 == 1) {
                val marker = (adjustedHashes(i) % 4) / 2
                val previousMarker = (adjustedHashes(i - 1) % 4) / 2
                if (marker >= 1 && previousMarker >= 1) {
                    valid = false
                }
            }
        }
        valid
    }
    
    def isSwapPair(hash1: Int, hash2: Int) = {
        val marker1 = (hash1 % 4) / 2
        val marker2 = (hash2 % 4) / 2
        (hash1 % 2) == 1 && (hash2 % 2) == 1 && marker1 != marker2
    }
    
    def applySwaps(hashes: Array[Int], i: Int, swapIndices: Array[Int]) = {
        val hashes1 = hashes.clone
        var bit = 1
        val n = hashes.length
        for (iBit <- swapIndices.indices) {
            if ((i & bit) != 0) {
                val i1 = swapIndices(iBit)
                val i2 = (i1 - 1 + n) % n
                val hash = hashes1(i1)
                hashes1(i1) = hashes1(i2)
                hashes1(i2) = hash
            }
            bit = bit << 1
        }
        hashes1
    }
    
    def getSwapVariants(hashes: Array[Int]) = {
        // Explores all "swap operations" "xxa2-a4yy" <-> "xxa4-a2yy"
        // "swap index" 0 is only allowed when eList.size = totalLength (swaps with the last element)
        val swapIndices = ArrayBuffer[Int]()
        if (eList.size == totalLength && isSwapPair(hashes(0), hashes.last)) {
            swapIndices += 0
        }
        for (i <- 1 until eList.length) {
            if ((i + startIndex) % 2 == 0 && isSwapPair(hashes(i - 1), hashes(i))) {
                swapIndices += i
            }
        }
        val variants = ArrayBuffer[Array[Int]]()
        for (i <- 0 until (1 << swapIndices.size)) {
            variants += applySwaps(hashes, i, swapIndices.toArray)
        }
        variants.toArray            
    }
    
    def assignMarkers = {
        // For each element of eSubList, find the marker:
        //   - for "opposing" (2nd part), take the marker from previous element: xxa1-[a2]yy -> xxa3-a4yy
        //   - for "matching" or "opposing" (1st part), increment the use count for the element: xxb1-[b1]yy -> xxb1-b3yy
        val hashes = eList.map(SymmetricCore.toHash)
        val counts = Array.fill(fullEdgeTypeCount * 4)(0)
        var valid = true
        for ((hash, i) <- hashes.zipWithIndex) {
            if (valid) {
                if (i > 0 && (i + startIndex) % 2 == 0 && hashes(i - 1) % 4 != hash % 4) {
                    val previousMarker = (hashes(i - 1) % 4) / 2
                    hashes(i) = hash + previousMarker * 2
                } else {
                    hashes(i) += 2 * counts(hash)
                }
                counts(hashes(i)) += 1
                if (counts(hashes(i)) > 1) {
                    valid = false
                }
            }
        }
        if (valid && isValid(counts, hashes)) {
            for (swappedHashes <- getSwapVariants(hashes)) yield {
                SymmetricCore(swappedHashes.map(SymmetricCore.fromHash).toArray, startIndex, totalLength, fullEdgeTypeCount)
            }
        } else {
            Array[SymmetricCore]()
        }
    }

    override def angleStrs = {
        val eFullList = Array.fill[String](totalLength)(null)
        for (i <- 0 until eList.length) {
            eFullList(i + startIndex) = eList(i)
        }
        assert((totalLength == eList.length && startIndex == 0) || totalLength == eList.length * 2)
        if (totalLength != eList.length) {
            for (i <- 0 until eList.length) {
                eFullList((startIndex - 1 - i + totalLength) % totalLength) = eList(i)
            }
        }
        for (i <- 0 until totalLength / 2) yield eFullList(2 * i) + eFullList(2 * i + 1)
    }
    
}

object SymmetricCore {
    
    def isSymmetric(eList: Seq[String], i: Int) = {
        val n = eList.size
        Array.range(0, n / 2).find(j => eList(i + j) != eList((i - j - 1 + n) % n)) match {
            case None => true
            case _ => false
        }
    }
    
    def toHash(s: String) = (s(0) - 'a') * 4 + (s(1) - '1')
    def fromHash(hash: Int) = "" + ('a' + hash / 4).toChar + ('1' + hash % 4).toChar
    
    def getVariants(eList: Seq[String]) = {
        val results = ArrayBuffer[(Array[String], Int)](eList.toArray -> 0)
        for (i <- 0 until eList.size / 2) {
            if (isSymmetric(eList, i)) {
                results += eList.slice(i, i + eList.size / 2).toArray -> i
            }
        }
        results.toArray
    }
    
    def getCoreFirstIndex(polyScheme: PolyScheme) = {
        val eList = polyScheme.toEList
        var firstIndex: Option[Int] = None
        for (i <- 0 until eList.size / 2) {
            if (firstIndex == None && isSymmetric(eList, i)) {
                firstIndex = Some(i)
            }
        }
        firstIndex
    }
    
    def explore(polyScheme: PolyScheme) = {
        val eList = polyScheme.toEList
        for ((eSubList, startIndex) <- getVariants(eList)) yield {
            SymmetricCore(eSubList, startIndex, eList.size, polyScheme.edgeTypeCount) 
        }
    }
}
