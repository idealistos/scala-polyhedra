package main.value

object Implicits {
    val Tolerance = 1e-8
        
    implicit def doubleToValue(d: Double) = new Constant(d)
    
    def sqrt(v: Value) = Sqrt(v).simplify
    def sqr(v: Value) = Sqr(v).simplify
    def cos(v: Value) = Cos(v).simplify
    def acos(v: Value) = Acos(v).simplify
 
    def cn(n: Int) = {
        assert(n >= 3)
        math.cos(math.Pi * (n - 2) / n)
    }
    
    def cp(a: Value, b: Value, c: Value) = PlanarCosine(a, b, c).simplify
    
    def cs(a: Value, b: Value, c: Value) = SphericalCosine(a, b, c).simplify
    
}