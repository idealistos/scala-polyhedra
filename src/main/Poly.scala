package main

import scala.collection.mutable.{ Map => MMap, HashMap => MHashMap, HashSet => MHashSet }
import scala.runtime.ScalaRunTime
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import ValueImplicits._

case class FacesDescriptor(count: Int) {

    case class Inner(facesDescriptor: FacesDescriptor) {
        def at(vertex: Vertex) {
            PolyContext.stack.last.setFaceCount(vertex, count)
        }
    }

    def faces() = Inner(this)
}

object Implicits {
    implicit def toFacesDescriptor(x: Int) = FacesDescriptor(x)
}

class Relation(val o: Poly, val subFace1: Poly, val subFace2: Poly, val reverse: Boolean) {

    def putValue(rank: Int, subFace: Poly, vertex: Vertex, value: Value) {
        vertex.nb(rank) = subFace.locateVertex(vertex, reverse)
        vertex.nbValue(rank) = value
    }

    def at(value: Value) {
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

}

abstract class Poly() extends Product with DelayedInit {
    val o: Poly = if (PolyContext.stack.size <= 1) null else PolyContext.stack(PolyContext.stack.size - 2)
    val rank: Int = if (o == null) 0 else o.rank + 1
    val byName = new MHashMap[String, Poly]
    val vList = new ArrayBuffer[Vertex]
    var name: String = null
    val faceCountsByVertex = new ArrayBuffer[(Vertex, Int)]
    val iFirstVar = PolyContext.iNextVar
    val localVars = new MHashMap[Int, Variable]

    override def delayedInit(x: => Unit) {
        PolyContext.stack += this
        x
        if (PolyContext.iNextVar > iFirstVar && !hasExternalVariables) {
            solveForVariables()
        }
        PolyContext.stack.remove(PolyContext.stack.size - 1)
    }

    def registered = !byName.isEmpty

    def registerFields() {
        val rm = scala.reflect.runtime.currentMirror
        val accessors = rm.classSymbol(getClass).toType.decls.collect {
            case f: MethodSymbol if f.isGetter && f.isPublic && f.name != "o" => f
        }
        val oMirror = rm.reflect(this)
        for (accessor <- accessors) {
            val face = oMirror.reflectMethod(accessor).apply()
            face match {
                case f: Poly => {
                    f.name = accessor.name.decodedName.toString
                    byName(f.name) = f
                    vList ++= f.vList
                }
                case _ =>
            }
        }
    }

    def getName(f: Poly) = byName.filter(_._2 eq f).head._1

    def getNameInParent(parentRank: Int): String =
        if (parentRank == rank - 1) name else o.getNameInParent(parentRank)

    def getFaceParent: Poly = if (rank == 1) this else o.getFaceParent

    def locateVertex(vertex: Vertex, reversed: Boolean): Vertex = {
        val nameInThis = vertex.getNameInParent(rank)
        byName(nameInThis).locateVertex(vertex, false)
    }

    def setFaceCount(vertex: Vertex, count: Int) {
        faceCountsByVertex.append((vertex, count))
    }

    def findPolygon(vertex: Vertex, faceCount: Int) = {
        // sides(0) is vertex.nbValue(1)
        // cosines(0) = cos of angle between sides(0) and sides(1), etc., taken from vertex.nbValue(2) or vertex.nbVar(2)
        val sides = new Array[Value](faceCount)
        val cosines = new Array[Value](faceCount)
        var v = vertex
        for (i <- 0 until faceCount) {
            var vNextSide = v.nb(1)
            sides(i) = v.nbValue(1)
            v = vNextSide.nb(2)
            cosines(i) = vNextSide.nbValue(2)
        }
        assert(v == vertex)
        SphericalPolygon(sides, cosines)
    }

    def hasExternalVariables: Boolean = {
        for (v <- vList) {
            for (i <- 0 until v.rank - rank) {
                if (v.nbValue(i).hasVariable(0, iFirstVar)) {
                    return true
                }
            }
        }
        false
    }

    def solveForVariables() {
        val sphericalPolygons = for ((vertex, faceCount) <- faceCountsByVertex) yield findPolygon(vertex, faceCount)
        val varArray = SphericalPolygonSolver.solve(sphericalPolygons)
        for (vertex <- vList) {
            for ((value, i) <- vertex.nbValue.zipWithIndex if value != null) {
                vertex.nbValue(i) = value.evaluate(varArray)
            }
        }
    }

    def ~~[P1 <: Poly, P2 <: Poly](poly1: P1, poly2: P2)(implicit u: P1 =:= P2) = {
        assert(poly1 == poly2)
        new Relation(this, poly1, poly2, false)
    }

    def ~#[P1 <: Poly, P2 <: Poly](poly1: P1, poly2: P2)(implicit u: P1 =:= P2) = {
        assert(poly1 == poly2)
        new Relation(this, poly1, poly2, true)
    }

    override def toString = ScalaRunTime._toString(this) + (if (o == null) "" else " of " + o)

}

object PolyContext {
    val stack = new ArrayBuffer[Poly]()
    var iNextVar = 0
}

case class Vertex() extends Poly() {
    val nb = new Array[Vertex](rank)
    val nbValue = new Array[Value](rank)
    val nbRealValue = new Array[Double](rank)

    vList += this

    override def locateVertex(vertex: Vertex, reversed: Boolean) = this
}

case class Edge(d: Value) extends Poly() {
    val vertex = Vertex()

    registerFields()
    vertex.nb(0) = vertex
    vertex.nbValue(0) = d

    override def locateVertex(vertex: Vertex, reversed: Boolean) = this.vertex
}

case class Side(d: Value) extends Poly() {
    val vertex1 = Vertex()
    val vertex2 = Vertex()

    registerFields()
    vertex1.nb(0) = vertex2
    vertex1.nbValue(0) = d
    vertex2.nb(0) = vertex1
    vertex2.nbValue(0) = d

    override def locateVertex(vertex: Vertex, reversed: Boolean): Vertex = {
        if ((vertex1.name == vertex.name) == reversed) vertex2 else vertex1
    }
}
