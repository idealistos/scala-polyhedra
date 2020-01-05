package main.scheme

final case class AngleMismatch() extends Exception

class VarContext {
    var iNextVar = 0
    def getNextVar = {
        val iVar = iNextVar
        iNextVar += 1
        iVar
    }
}

case class EdgeType(iEdge: Int, isSide: Boolean, marker: Int) {
    // E.g., c1 has iEdge = 2, marker = 1
    def markerSuffix = if (marker == 0) "" else "" + marker
    def name = (if (isSide) "side" else "edge") + ('A' + iEdge).toChar + markerSuffix
    def dName = "d" + ('A' + iEdge).toChar
    def className = if (isSide) "Side" else "Edge"
    def toCode = s"val $name = $className($dName)"
    
    def getVertexName(direction: Int) = {
        if (isSide) {
            if (direction == 0) "vertex1" else "vertex2"
        } else {
            "vertex"
        }
    }

}

case class RelationLine(fName1: String, fName2: String, fFaceName1: String,
        fFaceName2: String, value: String) {
    def toCode = {
        val innerCode = s"$fName1.$fFaceName1 matches $fName2.$fFaceName2"
        if (value == null) {
            innerCode
        } else {
            s"($innerCode) -> $value"
        }
    }
}

case class FactorInfo(factor: Int, extraConstraintCount: Int, iNextVar: Int)

