package main.scheme

import main.poly.PolyWrapper
import main.geometry.PolyGeometry
import main.drawing.PolyDrawing
import main.drawing.PolyDrawingInfo
import main.geometry.PolyData
import scala.util.Random
import main.value.Constant
import main.value.Value
import scala.collection.mutable.ArrayBuffer
import main.poly.PolyContext

class Instantiator(polyWrapper: PolyWrapper) {
    def tryToCalculate = {
        var found = false
        var iTry = 0
        var result: Option[PolyGeometry] = None
        while (!found && iTry < 20) {
            val sideParameters = Instantiator.getRandomValues(polyWrapper.sideParameterCount, 0.5, 2.5)
            val cosineParameters = Instantiator.getRandomValues(polyWrapper.cosineParameterCount, -0.8, 0.8)
            val params: Array[Value] = (sideParameters ++ cosineParameters).map(Constant(_))
            try {
                println(s"\n\n\nAttempt $iTry for ${polyWrapper.polyClassName} with ${params.mkString(", ")}")
                PolyContext.reset()
                val poly = polyWrapper.polyFunction(params)
                println(s"${poly.getClass.getSimpleName} found")
                val polyGeometry = PolyGeometry(poly)
                result = Some(polyGeometry)
                found = true
            } catch {
                case e: Exception => {
                    println(s"Cannot instantiate: $e")
                    found = false
                }
            }
            iTry += 1
        }
        result
    }
}

object Instantiator {
    val random = Random
    
    def getRandomValues(n: Int, minValue: Double, maxValue: Double) = {
        Array.fill(n)(minValue + (maxValue - minValue) * random.nextDouble)
    }
    
    def generateSVG(polyWrappers: Seq[PolyWrapper]) = {
        val svgParts = ArrayBuffer[(String, String)]()
        val missingClasses = ArrayBuffer[String]()
        for (polyWrapper <- polyWrappers) {
            new Instantiator(polyWrapper).tryToCalculate match {
                case Some(polyGeometry: PolyGeometry) => {
                    val polyData = PolyData(polyGeometry)
                    val className = polyGeometry.polyClassName
                    svgParts += className -> PolyDrawing.drawFacesAndLines(PolyDrawingInfo(polyData))
                }
                case None => missingClasses += polyWrapper.polyClassName
            }
        }
        missingClasses.foreach(println)
        svgParts
    }
}