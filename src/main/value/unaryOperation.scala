package main.value
import Implicits._

abstract class UnaryOperation extends Value {
    override def simplify = this match {
        case UnaryOperation(Constant(d)) => f(d)
        case _ => this
    }
    
    def v: Value
    def f(d: Double): Double
    def df(d: Double): Double
    def applyTo(x: Value): Value
    
    override def evaluate(varArray: Array[Double]) = f(v.evaluate(varArray))
    override def substitute(varArray: Array[Value]) = applyTo(v.substitute(varArray))
    override def differentiate(iVar: Int, varArray: Array[Double]) = v.differentiate(iVar, varArray) * df(v.evaluate(varArray))
    override def compute(varArray: Array[Double]) = {
        val vComputed = v.compute(varArray)
        val dfFactor = df(vComputed.f)
        Computed(f(vComputed.f), for (vDF <- vComputed.dfs) yield dfFactor * vDF)
    }
    override def hasVariable(iFrom: Int, iTo: Int) = v.hasVariable(iFrom, iTo)
    override def isConstant = v.isConstant
    override def varCount = v.varCount
}

object UnaryOperation {
    def unapply(value: UnaryOperation) = Some(value.v)
}

case class Negate(v: Value) extends UnaryOperation {
    override def f(d: Double) = -d
    override def df(d: Double) = -1.0
    override def applyTo(x: Value) = -x
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
    
    override def applyTo(x: Value) = sqrt(x)
}

case class Sqr(v: Value) extends UnaryOperation {
    override def f(d: Double) = d * d
    override def df(d: Double) = 2.0 * d
    
    override def applyTo(x: Value) = sqr(x)
}

case class Cos(v: Value) extends UnaryOperation {
    override def f(d: Double) = math.cos(d)
    override def df(d: Double) = -math.sin(d)
    override def applyTo(x: Value) = cos(x)
}

case class Acos(v: Value) extends UnaryOperation {
    override def f(d: Double) = math.acos(d)
    override def df(d: Double) = -1.0 / math.sqrt(1.0 - d * d)
    override def applyTo(x: Value) = acos(x)
}