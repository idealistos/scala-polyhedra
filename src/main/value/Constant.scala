package main.value

case class Constant(d: Double) extends Value {
    override def evaluate(varArray: Array[Double]) = d
    override def substitute(varArray: Array[Value]) = this
    override def differentiate(iVar: Int, varArray: Array[Double]) = 0.0
    override def compute(varArray: Array[Double]) = Computed(d, for (a <- varArray) yield 0.0)
    override def hasVariable(iFrom: Int, iTo: Int) = false
    override def isConstant = true
    override def varCount = 0
}
