package main.value

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