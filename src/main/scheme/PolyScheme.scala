package main.scheme

abstract class PolyScheme {
    def angleStrs: Seq[String]

    val edgeTypeCount = (for {
        s <- angleStrs
        i <- 0 to 1
    } yield (s(i * 2) - 'a')).max + 1
    
    lazy val edgeTypeVs = {
        // 0 (unused), 1 (symmetric), 3 (asymmetric), or 2 (symmetric but non-normalized)
        val typeVs = Array.fill[Int](edgeTypeCount)(0)
        for (s <- angleStrs; i <- 0 to 1) {
            typeVs(s(i * 2) - 'a') |= (s(i * 2 + 1) - '1') % 2 + 1
        }
        typeVs
    }
    
    lazy val edgeMatchTypes = {
        // 0 (not used), 1 (only "matches"), 2 (only "opposes"), 3 (mixed - not allowed)
        val matchTypes = Array.fill[Int](edgeTypeCount)(0)
        for (i <- angleStrs.indices) {
            val iNext = (i + 1) % angleStrs.length
            val m1 = (angleStrs(i)(3) - '1') % 2
            val m2 = (angleStrs(iNext)(1) - '1') % 2
            matchTypes(angleStrs(i)(2) - 'a') |= (if (m1 == m2) 1 else 2)
        }
        matchTypes
    }
    
    def toEList = {
        for {
            angle <- angleStrs
            j <- 0 to 1
        } yield angle.slice(2 * j, 2 * j + 2)
    }
    
}
