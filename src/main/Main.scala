package main

object CosineMethods {
    def cp(a: Double, b: Double, c: Double) = {
        assert(a > 0.0 && b > 0.0 && c >= 0)        
        (a * a + b * b - c * c) / (2.0 * a * b)
    } ensuring (v => math.abs(v) < 1.0)
    
    def cn(n: Int) = {
        assert(n >= 3)
        math.cos(math.Pi * (n - 2) / n)
    }
    
    def cs(ca: Double, cb: Double, cc: Double) = {
        assert(math.abs(ca) < 1.0)
        assert(math.abs(cb) < 1.0)
        assert(math.abs(cc) < 1.0)
        (cc - ca * cb) / (math.sqrt(1.0 - ca * ca) * math.sqrt(1.0 - cb * cb))
    } ensuring (v => math.abs(v) < 1.0)
    
}

import ValueImplicits._
import weka.core.Optimization
import weka.core.ConjugateGradientOptimization

class WekaTest extends ConjugateGradientOptimization {
    override def objectiveFunction(x: Array[Double]) = math.pow(x(0) - 1.0, 2.0) + math.pow(x(1) - 3.0, 2.0)
    override def evaluateGradient(x: Array[Double]) = Array(2.0 * (x(0) - 1.0), 2.0 * (x(1) - 3.0))
    override def getRevision = ""
}

object Test1 {
    abstract class Value {
        def basicEvaluate(varArray: Array[Double]): Double
        def evaluate(varArray: Array[Double]) = basicEvaluate(varArray)
    }
    
    case class Constant(d: Double) extends Value {
        override def basicEvaluate(varArray: Array[Double]) = d
    }
    
    case class Variable(i: Int) extends Value {    
        override def basicEvaluate(varArray: Array[Double]) = varArray(i)
    }
    
    case class Add(v1: Value, v2: Value) extends Value {
        override def basicEvaluate(varArray: Array[Double]) = v1.evaluate(varArray) + v2.evaluate(varArray)
    }
        
    trait UsingCache extends Value {
        var cached: Option[Double] = None
        override def evaluate(varArray: Array[Double]) = {
            if (cached == None) {
                cached = Some(basicEvaluate(varArray))
            }
            cached.get
        }
    }

    def main() {
        val expr = new Variable(0) with UsingCache
        val expr2 = new Add(expr, expr) with UsingCache
        println(expr2.evaluate(Array(5.0)))
    }
    
}

object Main {
 
    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block
        val t1 = System.nanoTime()
        println("Elapsed time: " + ((t1 - t0) / 1e9) + " s")
        result
    }
    
    def testWeka() {
        val test = new WekaTest
        var x = Array(0.0, 0.0)
        val constraints = for (i <- Array.range(0, 2)) yield Array(Double.NaN, Double.NaN)
        x = test.findArgmin(x, constraints)
        while (x == null) {
            x = test.getVarbValues()
            x = test.findArgmin(x, null)
        }
        val minFunction = test.getMinFunction();
        println(minFunction)
    }

    def main(args: Array[String]): Unit = {
        // val x = new NCube(3, 1.0)
        // val poly = new TruncatedRhombicosidodecahedron(0.8, 0.8, 0.1, 0.2)
        // val poly = new Cuboctahedron(1.0)
        // Test1.main()
        val poly = new RhombicDodecahedron(1.0)
        // val poly = new Tetrahedron(1.0)
        // val poly = new SnubCube(1.0, 1.0, 1.0)
        val polyData = PolyConverter3D(PolyGeometry(poly))
        print(PolyDrawing.drawFacesAndLines(PolyDrawingInfo(polyData)))
        println()
    }
    
}

