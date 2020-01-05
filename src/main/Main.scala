package main
import java.io.PrintWriter

import main.poly.PolyWrapperSamples
import main.scheme.Instantiator
import main.scheme.Uniform3DPolyhedraExplorer
import main.poly.PolyWrapper
import main.poly.PolyWrapperSamples4

object Main {
 
    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block
        val t1 = System.nanoTime()
        println("Elapsed time: " + ((t1 - t0) / 1e9) + " s")
        result
    }
    
    def generateAndSaveSVG(wrappers: Seq[PolyWrapper], filename: String) {
        val svgParts = Instantiator.generateSVG(wrappers)
        new PrintWriter(filename) {
            for ((polyClassName, svg) <- svgParts) {
                write(s"$polyClassName<br/>\n$svg<br>\n")
            }
            close()
        }
    }
    
    def main(args: Array[String]) {
        // testWeka();
        // val x = new NCube(3, 1.0)
        // val poly = new TruncatedRhombicosidodecahedron(0.8, 0.8, 0.1, 0.2)
        // val poly = new Cuboctahedron(1.0)
        // Test1.main()
        // val poly = new RhombicDodecahedron(1.0)
        // val poly = new Tetrahedron(1.0)
        // val poly = new Cube(1.0)
        // val poly = new SnubCube(1.0, 0.7, 0.7)
        // val poly = new SnubDodecahedron(1.0, 0.9, 0.8)
        // val poly = new RhombicDodecahedron(1.0)
        // val poly = new Icosahedron(1.0)
        // val poly = new IsoscelesTetrahedron(1.0, 2.0)
        // val poly = new TruncatedIcosidodecahedron(1.0, 0.7, 0.7)
        // val poly = new NSidedPyramid(5, 1.0, 5.0)
        // val poly = new CutHypercube(1.0)
        // val poly = new HyperTetrahedron(1.0)
        // val poly = new HyperCube(1.0)
        // val poly = new HyperPyramidOnCube(1.0, 1.0)
        // val poly = new HyperDodecahedron(1.0)
        // val poly = new RhombicDodecahedron2(1.0)
        // val poly = new FaceAP(1.0, 0.8)
        // val poly = new UniformTilingWith2Faces3EdgesScheme1Factors10(0.7, 0.8, 1.0) - cannot solve octagon
//        val poly = new UniformTilingWith1Face3EdgesScheme2Factors3(0.6, 0.8, 1.1, -0.2, -0.3)
////        val poly = new PrismOnIsoscelesTetrahedron(0.8, 0.9, 1.0)
//        val polyData = PolyData(PolyGeometry(poly))
//        print(PolyDrawing.drawFacesAndLines(PolyDrawingInfo(polyData)))
//        println()
//        Uniform3DPolyhedraExplorer.printScalaCode
        generateAndSaveSVG(PolyWrapperSamples4.wrappers, "poly3D-4angles.svg_parts") 
    }
    
}

