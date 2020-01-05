package main.poly

import main.value.Value

case class Vertex() extends Poly() {
    val nb = new Array[Vertex](rank)
    val nbValue = new Array[Value](rank)

    vList += this

    override def locateVertex(vertex: Vertex, reversed: Boolean) = this
}

case class Edge(d: Value) extends Poly() {
    val vertex = Vertex()

    registerFields()
    vertex.nb(0) = vertex
    vertex.nbValue(0) = d
}

case class Side(d: Value) extends Poly() {
    val vertex1 = Vertex()
    val vertex2 = Vertex()

    registerFields()
    vertex1.nb(0) = vertex2
    vertex1.nbValue(0) = d
    vertex2.nb(0) = vertex1
    vertex2.nbValue(0) = d
    opposeMapping(vertex1.name) = vertex2
    opposeMapping(vertex2.name) = vertex1

}
