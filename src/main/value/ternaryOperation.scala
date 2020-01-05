package main.value
import Implicits._

abstract class TernaryOperation extends Value {
    def v1: Value
    def v2: Value
    def v3: Value
    def f(d1: Double, d2: Double, d3: Double): Double
    def df1(d1: Double, d2: Double, d3: Double): Double
    def df2(d1: Double, d2: Double, d3: Double): Double
    def df3(d1: Double, d2: Double, d3: Double): Double
    def applyTo(x1: Value, x2: Value, x3: Value): Value
    
    override def simplify = this match {
        case TernaryOperation(Constant(d1), Constant(d2), Constant(d3)) => f(d1, d2, d3)
        case _ => this
    }
    
    override def evaluate(varArray: Array[Double]) = f(v1.evaluate(varArray), v2.evaluate(varArray), v3.evaluate(varArray))
    override def substitute(varArray: Array[Value]) = applyTo(v1.substitute(varArray), v2.substitute(varArray), v3.substitute(varArray))

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
    override def isConstant = v1.isConstant && v2.isConstant && v3.isConstant
    override def varCount = math.max(v1.varCount, math.max(v2.varCount, v3.varCount))
}

object TernaryOperation {
    def unapply(value: TernaryOperation) = Some(value.v1, value.v2, value.v3)
}

case class PlanarCosine(v1: Value, v2: Value, v3: Value) extends TernaryOperation {
    override def f(d1: Double, d2: Double, d3: Double) = {
        if (math.abs(d1) < Tolerance || math.abs(d2) < Tolerance) {
            Double.NaN
        } else {
            val value = (d1 * d1 + d2 * d2 - d3 * d3) / (2.0 * d1 * d2)
            if (math.abs(value) >= 1.0) {
                Double.NaN
            } else {
                value
            }
        }
    }

    override def df1(d1: Double, d2: Double, d3: Double) = {
        1.0 / (2.0 * d2) - ((d2 * d2 - d3 * d3) / (2.0 * d1 * d1 * d2))
    }

    override def df2(d1: Double, d2: Double, d3: Double) = {
        1.0 / (2.0 * d1) - ((d1 * d1 - d3 * d3) / (2.0 * d2 * d2 * d1))
    }

    override def df3(d1: Double, d2: Double, d3: Double) = {
        -d3 / (d1 * d2)
    }
    
    override def applyTo(x1: Value, x2: Value, x3: Value) = cp(x1, x2, x3)

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
    
    override def applyTo(x1: Value, x2: Value, x3: Value) = cs(x1, x2, x3)

}
