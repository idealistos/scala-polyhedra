package main.value

case class Variable(i: Int) extends Value {    
    override def evaluate(varArray: Array[Double]) = varArray(i)
    override def substitute(varArray: Array[Value]) = if (varArray(i) != null) varArray(i) else this
    override def differentiate(iVar: Int, varArray: Array[Double]) = if (i == iVar) 1.0 else 0.0
    
    override def compute(varArray: Array[Double]) = {
        Computed(varArray(i), for (iVar <- varArray.indices.toArray) yield (if (i == iVar) 1.0 else 0.0))
    }
    
    override def hasVariable(iFrom: Int, iTo: Int) = i >= iFrom && i < iTo
    override def varCount = i + 1
}
