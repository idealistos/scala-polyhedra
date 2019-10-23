package main

object ValueImplicits {
    implicit def doubleToValue(d: Double) = new Constant(d)
    
    def sqrt(v: Value) = Sqrt(v).simplify
    
    def sqr(v: Value) = Sqr(v).simplify
 
    def cn(n: Int) = {
        assert(n >= 3)
        math.cos(math.Pi * (n - 2) / n)
    }
    
    def cp(a: Value, b: Value, c: Value) = PlanarCosine(a, b, c).simplify
    
    def cs(a: Value, b: Value, c: Value) = SphericalCosine(a, b, c).simplify
    
    def x(iRelative: Int) = {
        PolyContext.stack.last.localVars.getOrElseUpdate(iRelative, {
            val i = PolyContext.iNextVar
            PolyContext.iNextVar += 1
            Variable(i)
        })
    }
    def x: Variable = x(0)
    def y: Variable = x(1)
    def z: Variable = x(2)
}

import ValueImplicits._

case class Computed(f: Double, dfs: Array[Double]) {
    def +(x: Computed) = Computed(f + x.f, for ((df, k) <- dfs.zipWithIndex) yield df + x.dfs(k))
    override def toString = f + " <" + (for (df <- dfs) yield f"$df%2.2f").mkString(" ") + ">"
}

abstract class Value {

    def unary_- = Negate(this)
    def +(v: Value) = Add(this, v).simplify
    def -(v: Value) = Subtract(this, v).simplify
    def *(v: Value) = Multiply(this, v).simplify
    def /(v: Value) = Divide(this, v).simplify
    
    def evaluate(varArray: Array[Double]): Double
    def differentiate(iVar: Int, varArray: Array[Double]): Double
    def compute(varArray: Array[Double]): Computed
    def simplify = this
    def hasVariable(iFrom: Int, iTo: Int): Boolean
    
    def constantValue = this match {
        case Constant(d) => d
        case _ => throw new IllegalArgumentException 
    }
    
    def varCount: Int
}

case class Constant(d: Double) extends Value {
    override def evaluate(varArray: Array[Double]) = d
    override def differentiate(iVar: Int, varArray: Array[Double]) = 0.0
    override def compute(varArray: Array[Double]) = Computed(d, for (a <- varArray) yield 0.0)
    override def hasVariable(iFrom: Int, iTo: Int) = false
    override def varCount = 0
}

case class Variable(i: Int) extends Value {    
    override def evaluate(varArray: Array[Double]) = varArray(i)
    override def differentiate(iVar: Int, varArray: Array[Double]) = if (i == iVar) 1.0 else 0.0
    
    override def compute(varArray: Array[Double]) = {
        Computed(varArray(i), for (iVar <- varArray.indices.toArray) yield (if (i == iVar) 1.0 else 0.0))
    }
    
    override def hasVariable(iFrom: Int, iTo: Int) = i >= iFrom && i < iTo
    override def varCount = i + 1
}

abstract class UnaryOperation extends Value {
    override def simplify = this match {
        case UnaryOperation(Constant(d)) => f(d)
        case _ => this
    }
    
    def v: Value
    def f(d: Double): Double
    def df(d: Double): Double
    
    override def evaluate(varArray: Array[Double]) = f(v.evaluate(varArray))
    override def differentiate(iVar: Int, varArray: Array[Double]) = v.differentiate(iVar, varArray) * df(v.evaluate(varArray))
    override def compute(varArray: Array[Double]) = {
        val vComputed = v.compute(varArray)
        val dfFactor = df(vComputed.f)
        Computed(f(vComputed.f), for (vDF <- vComputed.dfs) yield dfFactor * vDF)
    }
    override def hasVariable(iFrom: Int, iTo: Int) = v.hasVariable(iFrom, iTo)
    override def varCount = v.varCount
}

abstract class BinaryOperation extends Value {
    def v1: Value
    def v2: Value
    def f(d1: Double, d2: Double): Double
    def df1(d1: Double, d2: Double): Double
    def df2(d1: Double, d2: Double): Double
    
    override def simplify = this match {
        case BinaryOperation(Constant(d1), Constant(d2)) => f(d1, d2)
        case _ => this
    }
    
    override def evaluate(varArray: Array[Double]) = f(v1.evaluate(varArray), v2.evaluate(varArray))

    override def differentiate(iVar: Int, varArray: Array[Double]) = {
        val dv1 = v1.differentiate(iVar, varArray)
        val dv2 = v2.differentiate(iVar, varArray)
        val value1 = v1.evaluate(varArray)
        val value2 = v2.evaluate(varArray)
        dv1 * df1(value1, value2) + dv2 * df2(value1, value2)
    }
    
    override def compute(varArray: Array[Double]) = {
        val v1Computed = v1.compute(varArray)
        val v2Computed = v2.compute(varArray)
        val df1Factor = df1(v1Computed.f, v2Computed.f)
        val df2Factor = df2(v1Computed.f, v2Computed.f)
        val dfs = for (i <- varArray.indices.toArray) yield df1Factor * v1Computed.dfs(i) + df2Factor * v2Computed.dfs(i)
        Computed(f(v1Computed.f, v2Computed.f), dfs)                
    }

    override def hasVariable(iFrom: Int, iTo: Int) = v1.hasVariable(iFrom, iTo) || v2.hasVariable(iFrom, iTo)
    override def varCount = math.max(v1.varCount, v2.varCount)
}

abstract class TernaryOperation extends Value {
    def v1: Value
    def v2: Value
    def v3: Value
    def f(d1: Double, d2: Double, d3: Double): Double
    def df1(d1: Double, d2: Double, d3: Double): Double
    def df2(d1: Double, d2: Double, d3: Double): Double
    def df3(d1: Double, d2: Double, d3: Double): Double
    
    override def simplify = this match {
        case TernaryOperation(Constant(d1), Constant(d2), Constant(d3)) => f(d1, d2, d3)
        case _ => this
    }
    
    override def evaluate(varArray: Array[Double]) = f(v1.evaluate(varArray), v2.evaluate(varArray), v3.evaluate(varArray))

    override def differentiate(iVar: Int, varArray: Array[Double]) = {
        val dv1 = v1.differentiate(iVar, varArray)
        val dv2 = v2.differentiate(iVar, varArray)
        val dv3 = v3.differentiate(iVar, varArray)
        val value1 = v1.evaluate(varArray)
        val value2 = v2.evaluate(varArray)
        val value3 = v3.evaluate(varArray)
        dv1 * df1(value1, value2, value3) + dv2 * df2(value1, value2, value3) + dv3 * df3(value1, value2, value3)
    }
    
    override def compute(varArray: Array[Double]) = {
        val v1Computed = v1.compute(varArray)
        val v2Computed = v2.compute(varArray)
        val v3Computed = v3.compute(varArray)
        val df1Factor = df1(v1Computed.f, v2Computed.f, v3Computed.f)
        val df2Factor = df2(v1Computed.f, v2Computed.f, v3Computed.f)
        val df3Factor = df3(v1Computed.f, v2Computed.f, v3Computed.f)
        val dfs = for (i <- varArray.indices.toArray) yield {
            df1Factor * v1Computed.dfs(i) + df2Factor * v2Computed.dfs(i) + df3Factor * v3Computed.dfs(i)
        }       
        Computed(f(v1Computed.f, v2Computed.f, v3Computed.f), dfs)                
    }

    override def hasVariable(iFrom: Int, iTo: Int) = v1.hasVariable(iFrom, iTo) || v2.hasVariable(iFrom, iTo) || v3.hasVariable(iFrom, iTo)
    override def varCount = math.max(v1.varCount, math.max(v2.varCount, v3.varCount))
}

object UnaryOperation {
    def unapply(value: UnaryOperation) = Some(value.v)
}

object BinaryOperation {
    def unapply(value: BinaryOperation) = Some(value.v1, value.v2)
}

object TernaryOperation {
    def unapply(value: TernaryOperation) = Some(value.v1, value.v2, value.v3)
}

case class Negate(v: Value) extends UnaryOperation {
    override def f(d: Double) = -d
    override def df(d: Double) = -1.0
}

case class Sqrt(v: Value) extends UnaryOperation {
    override def f(d: Double) = {
        assert(d >= 0.0)
        math.sqrt(d)
    }
    
    override def df(d: Double) = {
        assert(d >= 0.0)
        1.0 / (2.0 * math.sqrt(d))
    }
    
}

case class Sqr(v: Value) extends UnaryOperation {
    override def f(d: Double) = d * d
    override def df(d: Double) = 2.0 * d
}

case class Add(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 + d2
    override def df1(d1: Double, d2: Double) = 1.0
    override def df2(d1: Double, d2: Double) = 1.0
}

case class Subtract(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 - d2
    override def df1(d1: Double, d2: Double) = 1.0
    override def df2(d1: Double, d2: Double) = -1.0
}

case class Multiply(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 * d2
    override def df1(d1: Double, d2: Double) = d2
    override def df2(d1: Double, d2: Double) = d1
}

case class Divide(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 / d2
    override def df1(d1: Double, d2: Double) = 1.0 / d2
    override def df2(d1: Double, d2: Double) = -d1 / (d2 * d2)
}

case class PlanarCosine(v1: Value, v2: Value, v3: Value) extends TernaryOperation {
    override def f(d1: Double, d2: Double, d3: Double) = {
        assert(d1 > 0.0 && d2 > 0.0 && d3 >= 0)
        ((sqr(d1) + sqr(d2) - sqr(d3)) / (2.0 * d1 * d2)).constantValue
    } ensuring (v => math.abs(v) < 1.0)

    override def df1(d1: Double, d2: Double, d3: Double) = {
        1.0 / (2.0 * d2) - ((sqr(d2) - sqr(d3)) / (2.0 * sqr(d1) * d2)).constantValue
    }

    override def df2(d1: Double, d2: Double, d3: Double) = {
        1.0 / (2.0 * d1) - ((sqr(d1) - sqr(d3)) / (2.0 * sqr(d2) * d1)).constantValue
    }

    override def df3(d1: Double, d2: Double, d3: Double) = {
        -d3 / (d1 * d2)
    }

}

case class SphericalCosine(v1: Value, v2: Value, v3: Value) extends TernaryOperation {
//            assert(math.abs(ca) < 1.0)
//        assert(math.abs(cb) < 1.0)
//        assert(math.abs(cc) < 1.0)
//        (cc - ca * cb) / (math.sqrt(1.0 - ca * ca) * math.sqrt(1.0 - cb * cb))
//    } ensuring (v => math.abs(v) < 1.0)
    override def f(ca: Double, cb: Double, cc: Double) = {
        assert(math.abs(ca) < 1.0)
        assert(math.abs(cb) < 1.0)
        assert(math.abs(cc) < 1.0)
        (cc - ca * cb) / (math.sqrt(1.0 - ca * ca) * math.sqrt(1.0 - cb * cb))
    } ensuring (v => math.abs(v) < 1.0)

    override def df1(ca: Double, cb: Double, cc: Double) = {
        f(ca, cb, cc) * (-cb / (cc - ca * cb) + ca / (1.0 - ca * ca))
    }

    override def df2(ca: Double, cb: Double, cc: Double) = {
        f(ca, cb, cc) * (-ca / (cc - ca * cb) + cb / (1.0 - cb * cb))
    }
    
    override def df3(ca: Double, cb: Double, cc: Double) = {
        1.0 / (math.sqrt(1.0 - ca * ca) * math.sqrt(1.0 - cb * cb))
    }

}
