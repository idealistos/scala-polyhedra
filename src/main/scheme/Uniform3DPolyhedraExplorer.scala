package main.scheme

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import main.CompilerImplicits._


object Uniform3DPolyhedraExplorer {
    // Edge types:
    //   A: Edge, both faces are equal
    //   B: Edge, faces are different
    //   C: matching Side, both faces are equal
    //   D: matching Side, faces are different
    //   E: opposing Side, both faces are equal
    //   F: opposing Side, faces are different
    // Examples:
    //   A: regular polyhedron -> a1a1
    //   B: cuboctahedron -> a1a1 (face A), a1a1 (face B)
    //   AB: truncated cube -> a1a1, (a1b1, b1a1)
    //   BBB: truncated cuboctahedron -> (a1b1, b1a1), (b1c1, c1b1), (c1a1, a1c1)
    //   E: oriented tetrahedron -> a1b1
    
    def listPolySchemes = {
        val polySchemes3 = ArrayBuffer[PolyScheme3]()
        for (angleCount <- 4 to 4) {
            for (edgeTypeCount <- 1 to 4) {
                val polySchemes1 = PolyScheme1.explore(angleCount, edgeTypeCount)
                runtime.ScalaRunTime.stringOf("abc")
                for (polyScheme1 <- polySchemes1) {
                    println("Scheme1: " + runtime.ScalaRunTime.stringOf(polyScheme1))
                    val polySchemes2 = PolyScheme2.explore(polyScheme1)
                    for (polyScheme2 <- polySchemes2) {
                        println("- Scheme2: " + runtime.ScalaRunTime.stringOf(polyScheme2))
                        val polyScheme3 = PolyScheme3(polyScheme2.angleStrs)
                        println("-- Scheme3: " + polyScheme3)
                        if (polyScheme3.isValid) {
                            polySchemes3 += polyScheme3
                        }
                    }
                }
            }
        }
        println(s"Found ${polySchemes3.size} poly schemes")
        polySchemes3
    }
    
    def getPolyClassesAndWrappers = {
        val polyClasses = ArrayBuffer[String]()
        val polyWrappers = ArrayBuffer[String]()
        val counter = new MHashMap[String, Int].withDefaultValue(0)
        val restriction = null
        // val restriction = "A(a1b1)A(b2c1)A(c2d1)A(d2a2)"
        // val restriction = "A(a1a1)A(a2b1)A(b2b2)A(b1a2)"        
        for (polyScheme3 <- listPolySchemes) {
            if (restriction == null || polyScheme3.getDescription == restriction) {
                for ((factorMap, i) <- polyScheme3.eulerChecker.getValidFactorMaps.zipWithIndex) {
                    val isTiling = polyScheme3.eulerChecker.isTiling(factorMap)
                    var eccArray = Array.fill(polyScheme3.faceTemplates.length)(0)
                    if (!isTiling) {
                        eccArray = polyScheme3.getExtraConstraintCounts
                    }
                    val polyType = if (isTiling) "Tiling" else "Poly"
                    val abbreviation = polyScheme3.getAbbreviation(factorMap)
                    val classNamePart = s"Uniform${polyType}${abbreviation}"
                    val index = counter(classNamePart)
                    counter(classNamePart) = index + 1
                    val className = if (index == 0) classNamePart else s"${classNamePart}V${index + 1}"
                    val polyClass = polyScheme3.toClassCode(className, factorMap, eccArray)
                    polyClasses += polyClass
                    polyWrappers += polyScheme3.toWrapperCode(className, factorMap, eccArray)
                }
            }
        }
        (polyClasses, polyWrappers)
    }
    
    def getScalaCode = {
        val (polyClasses, polyWrappers) = getPolyClassesAndWrappers
        val polyClassesStr = polyClasses.map(_.formattedScalaCode).mkString("\n\n")
        val wrappersStr = polyWrappers.mkString(",\n")
        s"$polyClassesStr\nVector($wrappersStr)"
    }

    def printScalaCode = {
        val (polyClasses, polyWrappers) = getPolyClassesAndWrappers
        val polyClassesStr = polyClasses.map(_.formattedScalaCode).mkString("\n\n")
        val wrappersStr = polyWrappers.map("            " + _).mkString(",\n") + ",\n"
        val comment = s"// ${polyWrappers.length} found"
        println(s"\n\n$polyClassesStr\n\nobject PolyWrapperSamples {\n    $comment\n    val wrappers = Vector(\n$wrappersStr    )\n}")
    }

}