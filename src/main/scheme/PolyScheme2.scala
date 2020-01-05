package main.scheme

import scala.collection.mutable.ArrayBuffer

case class PolyScheme2(angleStrs: Seq[String]) extends PolyScheme {
      
}

object PolyScheme2 {
    
    def explore(polyScheme1: PolyScheme1) = {
        // Given an "equivalence class" (e.g., a1a2-a1b1-b1a2), assign "markers" to every individual edge in an angle with the conditions:
        // - symmetry around a symmetric edge (e.g., b1-b1)
        // - count = 1 for each element (a1, a2, b1, etc.), except when symmetric, then 1 or 2 (as in a1b1-b1a1)
        // - "opposing" is allowed for the same "marker" only (i.e., ..a1-a2.. is OK but ..a1-a4.. is not)
        // - renaming (e.g., a1 <-> a3, a2 <-> a4) makes equivalent, so choose the minimum under all renamings
        // - changing an arrow's direction (e.g., a1 <-> a2) makes equivalent, so choose the minimum under all renamings
        // - all groups (e.g., a1, a2, a3, a4) are connected (so no a5, a6, etc.)
        // Examples:
        // a1b1-b1c1-c1a1 => a1b1-b3c1-c3a3
        // a1b1-b1c1-c1a2 => a1b1-b3c1-c3a2
        // a1b1-b1c1-c2a2 => a1b1-b3c1-c2a2
        // a1a1-a1b1-b1a1 => a1a1-a3b1-b1a3 (no a1a3-a5b1-b3a7), from "symmetric subset" a1-a1b1
        val polySchemes2 = ArrayBuffer[PolyScheme2]()
        for (core <- SymmetricCore.explore(polyScheme1)) {
            for (core2 <- core.assignMarkers) {
                val angleStrs2 = core2.angleStrs
                polySchemes2 += PolyScheme2(angleStrs2)
            }
        }
        polySchemes2
    }
}