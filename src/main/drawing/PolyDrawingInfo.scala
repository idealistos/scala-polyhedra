package main.drawing

import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import scala.util.Random
import java.awt.Color
import main.geometry.MyVec
import main.geometry.PolyData

case class PolyDrawingInfo(val polyData: PolyData, val matrix: Array[Array[Double]], val colorMap: Array[Color])

object PolyDrawingInfo {
    
    val random = Random
    def getRandomColor = Color.getHSBColor(random.nextFloat(), 0.6f, 1.0f)


    def rotateRandomly(m: Array[Array[Double]], i1: Int, i2: Int, c: Double) {
        val s = math.sqrt(1.0 - c * c)
        val v1 = new MyVec(m(i1)) * c + new MyVec(m(i2)) * s
        val v2 = new MyVec(m(i1)) * (-s) + new MyVec(m(i2)) * c
        m(i1) = v1.p
        m(i2) = v2.p
    }
    
    def getUnitMatrix(n: Int) = (for (i <- 0 until n) yield MyVec(n, i).p).toArray

    def getRandomRotationMatrix(n: Int) = {
        if (n == 1) {
            Array(Array(1.0))
        } else {
            val m = getUnitMatrix(n)
            val random = Random
            for (i <- 0 until 100) {
                val i1 = random.nextInt.abs % n
                val i2 = ((random.nextInt.abs % (n - 1) + 1) + i1) % n
                rotateRandomly(m, i1, i2, random.nextDouble)
            }
            m
        }
    }

    def apply(polyData: PolyData) = {
        val names = MHashSet[String]()
        for (face <- polyData.faces) {
            names.add(face.name)
        }
        val colorByName = MHashMap[String, Color]() ++ (for (name <- names) yield (name -> getRandomColor))
        val colorMap = for (face <- polyData.faces) yield colorByName(face.name)
        val rotationMatrix = if (polyData.needsRotationMatrix) {
            getRandomRotationMatrix(polyData.n)
        } else {
            getUnitMatrix(polyData.n)
        }
        new PolyDrawingInfo(polyData, rotationMatrix, colorMap)
    }
    
}
