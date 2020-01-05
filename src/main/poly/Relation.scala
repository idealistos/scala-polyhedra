package main.poly

import main.value.Value

class Relation(val o: Poly, val subFace1: Poly, val subFace2: Poly, val oppose: Boolean) {

    def putValue(rank: Int, subFace: Poly, vertex: Vertex, value: Value) {
        vertex.nb(rank) = subFace.locateVertex(vertex, oppose)
        vertex.nbValue(rank) = value
    }

    def registerConnection(value: Value) {
        if (!o.registered) {
            o.registerFields()
        }
        for (vertex <- subFace1.vList) {
            val i = vertex.rank - subFace1.rank + 1
            putValue(i, subFace2, vertex, value)
        }
        if (subFace1 ne subFace2) {
            for (vertex <- subFace2.vList) {
                val i = vertex.rank - subFace1.rank + 1
                putValue(i, subFace1, vertex, value)
            }
        }
    }
    
    def ->(value: Value) {
        PolyContext.iNextVar -= 1
        registerConnection(value)
    }
    
}
