package main.scheme

abstract class ReorderingRule {
    def applyTo(chars: Array[Char]): Unit
}

case class SwapLetterReorderingRule(c1: Char, c2: Char) extends ReorderingRule {
    override def applyTo(chars: Array[Char]) {
        for (i <- chars.indices) {
            if (chars(i) == c1) {
                chars(i) = c2
            } else if (chars(i) == c2) {
                chars(i) = c1
            }
        }
    }
}

case class SwapMarkerReorderingRule(pos: Int) extends ReorderingRule {
    override def applyTo(chars: Array[Char]) {
        val c = chars(pos)
        for (i <- 0 until chars.length / 2) {
            if (chars(2 * i) == c) {
                val pos1 = 2 * i + 1
                if (chars(pos1) == '1') {
                    chars(pos1) = '2'
                } else if (chars(pos1) == '2') {
                    chars(pos1) = '1'
                }
            }
        }
    }
}
