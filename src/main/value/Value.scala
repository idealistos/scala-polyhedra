package main.value

abstract class Value {

    def unary_- = Negate(this).simplify
    def +(v: Value) = Add(this, v).simplify
    def -(v: Value) = Subtract(this, v).simplify
    def *(v: Value) = Multiply(this, v).simplify
    def /(v: Value) = Divide(this, v).simplify
    
    def evaluate(varArray: Array[Double]): Double
    def substitute(varArray: Array[Value]): Value
    def differentiate(iVar: Int, varArray: Array[Double]): Double
    def compute(varArray: Array[Double]): Computed
    def simplify = this
    def hasVariable(iFrom: Int, iTo: Int): Boolean
    def isConstant = false
    
    def constantValue = this match {
        case Constant(d) => d
        case _ => throw new IllegalArgumentException 
    }
    
    def varCount: Int
}
