package main

import scala.collection.mutable.ArrayBuffer
import main.poly.Poly
import main.value.Value

object CompilerImplicits {
    
    case class ScalaString(s: String) {
        def formattedScalaCode = {
            val newLines = ArrayBuffer[String]()
            var indent = 0
            for (line <- s.split("\n")) {
                if (line.endsWith("}")) {
                    indent -= 4
                }
                newLines += (" " * indent) + line
                if (line.endsWith("{")) {
                    indent += 4
                } 
            }
            newLines.mkString("\n")
        }        
    }
    
    implicit def AsScalaString(s: String) = ScalaString(s)
}

class Compiler {
    
}