package main.value
import Implicits._

abstract class BinaryOperation extends Value {
    def v1: Value
    def v2: Value
    def f(d1: Double, d2: Double): Double
    def df1(d1: Double, d2: Double): Double
    def df2(d1: Double, d2: Double): Double
    def applyTo(x1: Value, x2: Value): Value
    
    override def simplify = this match {
        case BinaryOperation(Constant(d1), Constant(d2)) => f(d1, d2)
        case _ => this
    }
    
    override def evaluate(varArray: Array[Double]) = f(v1.evaluate(varArray), v2.evaluate(varArray))
    override def substitute(varArray: Array[Value]) = applyTo(v1.substitute(varArray), v2.substitute(varArray))

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
    override def isConstant = v1.isConstant && v2.isConstant
    override def varCount = math.max(v1.varCount, v2.varCount)
}

object BinaryOperation {
    def unapply(value: BinaryOperation) = Some(value.v1, value.v2)
}

case class Add(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 + d2
    override def df1(d1: Double, d2: Double) = 1.0
    override def df2(d1: Double, d2: Double) = 1.0
    override def applyTo(x1: Value, x2: Value) = x1 + x2
}

case class Subtract(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 - d2
    override def df1(d1: Double, d2: Double) = 1.0
    override def df2(d1: Double, d2: Double) = -1.0
    override def applyTo(x1: Value, x2: Value) = x1 - x2
}

case class Multiply(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 * d2
    override def df1(d1: Double, d2: Double) = d2
    override def df2(d1: Double, d2: Double) = d1
    override def applyTo(x1: Value, x2: Value) = x1 * x2
}

case class Divide(v1: Value, v2: Value) extends BinaryOperation {
    override def f(d1: Double, d2: Double) = d1 / d2
    override def df1(d1: Double, d2: Double) = 1.0 / d2
    override def df2(d1: Double, d2: Double) = -d1 / (d2 * d2)
    override def applyTo(x1: Value, x2: Value) = x1 / x2
}

