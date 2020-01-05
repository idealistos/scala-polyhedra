package main.value

case class Computed(f: Double, dfs: Array[Double]) {
    def +(x: Computed) = Computed(f + x.f, for ((df, k) <- dfs.zipWithIndex) yield df + x.dfs(k))
    def -(x: Computed) = Computed(f - x.f, for ((df, k) <- dfs.zipWithIndex) yield df - x.dfs(k))
    def unary_- = Computed(-f, for (df <- dfs) yield -df)
    def *(x: Computed) = Computed(f * x.f, for ((df, k) <- dfs.zipWithIndex) yield df * x.f + x.dfs(k) * f)
    
    def timesCos(c: Computed) = {
        // Returns c * this (c is interpreted as cos alpha) and its derivatives
        Computed(c.f * f, for ((df, k) <- dfs.zipWithIndex) yield c.dfs(k) * f + c.f * df)
    }
    
    def timesSin(c: Computed) = {
        // Returns sqrt(1 - c^2) * this (c is interpreted as cos alpha) and its derivatives
        val s = math.sqrt(1.0 - c.f * c.f)
        Computed(s * f, for ((df, k) <- dfs.zipWithIndex) yield c.dfs(k) * (-c.f / s) * f + s * df)
    }
    
    override def toString = f + " <" + (for (df <- dfs) yield f"$df%2.2f").mkString(" ") + ">"
}
