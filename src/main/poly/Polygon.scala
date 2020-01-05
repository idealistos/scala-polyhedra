package main.poly

import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import scala.collection.mutable.ArrayBuffer
import main.value.Variable
import main.geometry.MyVec
import main.value.Value
import main.value.Implicits._
import main.value.Constant

abstract class Polygon(m: Int) extends Poly() {
    
    def solveTriangle(sides: Array[Value], cosines: Array[Value]) = {
        cosines match {
            case Array(Variable(v1), Variable(v2), Variable(v3)) => {
                val iMax = math.max(math.max(v1, v2), v3)
                val varArray = Array.fill[Value](iMax + 1)(null)
                varArray(v1) = cp(sides(0), sides(1), sides(2))
                varArray(v2) = cp(sides(1), sides(2), sides(0))
                varArray(v3) = cp(sides(2), sides(0), sides(1))
                varArray
            }
            case _ => null
        }
    }
    
    def getTrapezoidCosine(side: Value, baseDifference: Value) = {
        if (baseDifference.isConstant && math.abs(baseDifference.constantValue) < Tolerance) {
            Constant(0.0)
        } else {
            cp(side, baseDifference, side)
        }
    }
    
    def solveTrapezoid(sides: Array[Value], cosines: Array[Value]): Array[Value] = {
        cosines match {
            case Array(Variable(v1), Variable(v1a), Variable(v2), Variable(v2a)) if v1 == v1a && v2 == v2a => {
                assert(sides(0) == sides(2))
                val varArray = Array.fill[Value](math.max(v1, v2) + 1)(null)
                varArray(v1) = getTrapezoidCosine(sides(0), sides(1) - sides(3))
                varArray(v2) = -varArray(v1)
                varArray
            }
            case Array(Variable(v1), Variable(v2), Variable(v2a), Variable(v1a)) if v1 == v1a && v2 == v2a => {
                solveTrapezoid(sides(3) +: sides.take(3), cosines(3) +: cosines.take(3))
            }
            case _ => null
        }
    }
    
    def findRemainingAngle(cosines: Array[Value]) = {
        // Finds x from the equation Sum acos(cosine(i)) = (m - 2) * pi / factor,
        // where m = factor * period (= cosines.length)
        cosines.collectFirst {
            case Variable(v) if v >= iFirstVar => v
        } match {
            case Some(v) if cosines.forall {
                    case Variable(`v`) => true
                    case value if !value.hasVariable(iFirstVar, PolyContext.iNextVar) => true
                    case _ => false
            } => {
                var angleSum: Value = 0.0
                var countV = 0
                for (c <- cosines) {
                    if (c == Variable(v)) {
                        countV += 1
                    } else {
                        angleSum += acos(c)
                    }
                }
                val factor = m / cosines.length
                val sumAll = (m - 2) * math.Pi / factor
                val angle = (sumAll - angleSum) / countV
                if (angle.isConstant && (angle.constantValue <= 0.0 || angle.constantValue >= math.Pi)) {
                    throw new Exception("Cannot solve the polygon - sum of angles cannot reach the value")
                }
                Array.fill[Value](v)(null) :+ cos(angle)
            }
            case _ => null
        }
    }
    
    def solvePlanarPolygon(sides: Array[Value], cosines: Array[Value]) = {
        val evaluators = Array(PlanarPolygonEvaluator(sides, cosines, PolyContext.iNextVar))
//        val sides1 = Array[Value](1.0, 1.0, 1.0, 1.0)
//        val cosines1 = Array[Value](Variable(0), Variable(0), Variable(0), Variable(0))
//        val evaluators = Array(PlanarPolygonEvaluator(sides1, cosines1, 1))
        PolygonSolver(evaluators).solve.map[Value, Array[Value]](Constant(_))
    }
    
    override def solveForVariables() {
        println("--- Collecting 2D polygon")
        val toSolve = findPolygon(this, m)
        var varArray: Array[Value] = null
        if (m == 3) {
            varArray = solveTriangle(toSolve.sides, toSolve.cosines)
        } else if (toSolve.cosines.forall(_ == toSolve.cosines(0))) {
            toSolve.cosines(0) match {
                case Variable(i) => varArray = Array.fill[Value](i)(null) :+ Constant(cn(m))
                case _ =>
            }
        } else if (m == 4 && ((toSolve.cosines(0) == toSolve.cosines(3) && toSolve.cosines(1) == toSolve.cosines(2))
                || (toSolve.cosines(0) == toSolve.cosines(1) && toSolve.cosines(2) == toSolve.cosines(3)))) {
            varArray = solveTrapezoid(toSolve.sides, toSolve.cosines)
        } else if (m > getAxisPeriod(this)) {
            val period = getAxisPeriod(this)
            varArray = findRemainingAngle(toSolve.cosines.slice(0, period))
        }
        if (varArray == null && toSolve.sides.forall(!_.hasVariable(0, iFirstVar))
                && toSolve.cosines.forall(!_.hasVariable(0, iFirstVar))) {
            varArray = solvePlanarPolygon(toSolve.sides, toSolve.cosines)
        }
        if (varArray == null) {
            throw new Exception("Cannot solve the polygon")
        }
        println(s"Solution: ${varArray.mkString(", ")}")
        substituteNbVariables(varArray, iFirstVar)
        findPolygon(this, m)
    }
    
    override def checkOpposable() {
        // Finds the mapping v -> v* such that (v.nb(i))* = v*.nb(i), i = 0 or 1
        val queue = ArrayBuffer[(Vertex, Vertex)]()
        val checkedNames = MHashSet[(String, String)]()
        
        def register(v: Vertex, vStar: Vertex) {
            val vNames = (v.getNameForRank(rank + 1), vStar.getNameForRank(rank + 1))
            if (!checkedNames.contains(vNames)) {
                queue += v -> vStar
                checkedNames += vNames
            }
        }
        
        try {
            for (v <- vList) {
                val v1 = v.nb(0)
                if (v1.name != v.name) {
                    register(v, v1)
                    register(v1, v)
                }
            }
            var index = 0
            while (index < queue.size) {
                val (v, vStar) = queue(index)
                val vName = v.getNameForRank(rank + 1)
                if (opposeMapping.contains(vName) && (opposeMapping.get(vName).get ne vStar)) {
                    throw NotOpposable()
                }
                opposeMapping(vName) = vStar
                for (i <- 0 to 1) {
                    register(v.nb(i), vStar.nb(i))
                }
                index += 1
            }
        } catch {
            case NotOpposable() => opposeMapping.clear()
        }
    }
    
    override def allowsExternalVariables = true
    
}
