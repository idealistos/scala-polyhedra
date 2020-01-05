package main.poly

import scala.runtime.ScalaRunTime
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import main.value.Variable
import main.value.Value
import main.value.Implicits._

abstract class Poly() extends Product with DelayedInit {
    val o: Poly = if (PolyContext.stack.size <= 1) null else PolyContext.stack(PolyContext.stack.size - 2)
    val rank: Int = if (o == null) 0 else o.rank + 1
    val byName = new MHashMap[String, Poly]
    val vList = new ArrayBuffer[Vertex]
    var name: String = null
    val faceCountsByAxis = new ArrayBuffer[(Poly, Int)]
    val iFirstVar = PolyContext.iNextVar
    val localVars = new MHashMap[Int, Variable]
    val opposeMapping = new MHashMap[String, Vertex]
    
    def getNameForRank(r: Int): String = if (r == rank || name == null) name else {
        val oName = o.getNameForRank(r)
        if (oName == null) name else oName + "." + name
    }

    def checkOpposable() { }
    def allowsExternalVariables = false

    override def delayedInit(body: => Unit) {
        PolyContext.stack += this
        body
        if (registered) {
            checkOpposable()
        }
        if (PolyContext.iNextVar > iFirstVar && (!hasExternalVariables || allowsExternalVariables)) {
            solveForVariables()
            PolyContext.iNextVar = iFirstVar
        }
        PolyContext.stack.remove(PolyContext.stack.size - 1)
    }

    def registered = !byName.isEmpty

    def registerFields() {
        val rm = scala.reflect.runtime.currentMirror
        val fields = rm.classSymbol(getClass).toType.decls.collect {
            case f: TermSymbol if f.isVal && f.name.toString != "$outer" => f
        }
        val oMirror = rm.reflect(this)
        for (field <- fields) {
            // val face = oMirror.reflectMethod(accessor).apply()
            val face = oMirror.reflectField(field).get
            face match {
                case f: Poly => {
                    f.name = field.name.decodedName.toString.trim
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
    def getSubFaceParent: Poly = if (rank == 2) this else o.getSubFaceParent

    def locateVertex(vertex: Vertex, reversed: Boolean): Vertex = {
        val nameInThis = vertex.getNameInParent(rank)
        val newVertex = byName(nameInThis).locateVertex(vertex, false)
        if (reversed) {
            assert(!opposeMapping.isEmpty && opposeMapping.contains(newVertex.getNameForRank(rank + 1)))
            opposeMapping(newVertex.getNameForRank(rank + 1))
        } else {
            newVertex
        }
    }

    def setCountByAxis(axisPoly: Poly, count: Int) {
        faceCountsByAxis += axisPoly -> count
    }

    def findPolygon(axisPoly: Poly, faceCount: Int) = {
        // sides(0) is vertex.nbValue(1)
        // cosines(0) = cos of angle between sides(0) and sides(1), etc., taken from vertex.nbValue(2) or vertex.nbVar(2)
        val sides = new Array[Value](faceCount)
        val cosines = new Array[Value](faceCount)
        var v = axisPoly.vList(0)
        val n = v.nb.length - rank
        for (i <- 0 until faceCount) {
            var vNextSide = v.nb(n - 2)
            sides(i) = v.nbValue(n - 2)
            v = vNextSide.nb(n - 1)
            cosines(i) = vNextSide.nbValue(n - 1)
        }
        assert(v.equalsRecursively(axisPoly.vList(0)))
        println(s"Sides (${sides.length}): " + sides.mkString(", "))
        println(s"Cosines(${cosines.length}): " + cosines.mkString(", "))
        PolygonElements(sides, cosines)
    }
    
    def getAxisPeriod(axisPoly: Poly) = {
        var v = axisPoly.vList(0)
        val v0 = v
        val n = v.nb.length - rank
        var i = 1
        while (!v.nb(n - 2).nb(n - 1).equalsRecursively(v0)) {
            v = v.nb(n - 2).nb(n - 1)
            i += 1
        }
        i
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
    
    def evaluateNbVariables(varArray: Array[Double], afterIVar: Int) {
        for (vertex <- vList) {
            for ((value, i) <- vertex.nbValue.zipWithIndex if value != null && !value.hasVariable(0, afterIVar)) {
                vertex.nbValue(i) = value.evaluate(varArray)
            }
        }
    }
    
    def substituteNbVariables(varArray: Array[Value], afterIVar: Int) {
        for (vertex <- vList) {
            for ((value, i) <- vertex.nbValue.zipWithIndex if value != null && !value.hasVariable(0, afterIVar)) {
                vertex.nbValue(i) = value.substitute(varArray)
            }
        }
    }
    
    def isTiling = vList.forall(v => math.abs(v.nbValue(v.rank - 1).constantValue + 1.0) < Tolerance)

    def solveForVariables() {
        println("--- Collecting spherical polygons")
        val varCount = PolyContext.iNextVar
        val sphericalPolygons = for ((axisPoly, faceCount) <- faceCountsByAxis) yield {
            SphericalPolygonEvaluator(findPolygon(axisPoly, faceCount), varCount)
        }
        if (!sphericalPolygons.forall(_.isValid)) {
            throw new Exception("Some spherical polygon has no solution")
        }
        val varArray = PolygonSolver(sphericalPolygons).solve
        evaluateNbVariables(varArray, iFirstVar)
    }
    
    def matchesOrOpposes[P1 <: Poly, P2 <: Poly](poly1: P1, poly2: P2, opposes: Boolean) = {
        assert(poly1 == poly2)
        val relation = new Relation(this, poly1, poly2, opposes)
        val variable = Variable(PolyContext.iNextVar)
        PolyContext.iNextVar += 1
        relation.registerConnection(variable)
        relation
    }

    def matchesAtAngle[P1 <: Poly, P2 <: Poly](poly1: P1, poly2: P2)(implicit u: P1 =:= P2) = {
        matchesOrOpposes(poly1, poly2, false)
    }

    def opposesAtAngle[P1 <: Poly, P2 <: Poly](poly1: P1, poly2: P2)(implicit u: P1 =:= P2) = {
        matchesOrOpposes(poly1, poly2, true)
    }
    
    def equalsRecursively(poly: Poly): Boolean = {
        if (o == null && poly.o == null) {
            this == poly && this.name == poly.name
        } else if (o == null || poly.o == null) {
            false
        } else {
            this == poly && this.name == poly.name && o.equalsRecursively(poly.o)
        }
    }
    
    override def toString = {
        val prefix = if (name == null) "" else getNameForRank(0) + " = "
        prefix + ScalaRunTime._toString(this) + (if (o == null) "" else " of " + o)
    }

}
