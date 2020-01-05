package main.drawing

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.BasicStroke
import java.awt.geom.Line2D
import scala.util.Random
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import org.jfree.graphics2d.svg.SVGGraphics2D
import main.geometry.PolyData

object PolyDrawing {

    def transform(m: Array[Array[Double]], v: Array[Double]) = {
        val n = v.size
        for (i <- Array.range(0, n)) yield {
            var s = 0.0
            for (j <- 0 until n) {
                s += v(j) * m(i)(j)
            }
            s
        }
    }

    def toPoint(v: Array[Double]) = new Point2D.Double((v(0) * 250) + 500, (v(1) * 250) + 300)

    def drawLine(g2: SVGGraphics2D, v1: Array[Double], v2: Array[Double]) {
        g2.draw(new Line2D.Double(toPoint(v1), toPoint(v2)))
    }
    
    def getPolygon(iPoints: Vector[Int], vs: Array[Array[Double]]) = {
        val polygon = new java.awt.Polygon()
        for (iPoint <- iPoints) {
            val point = toPoint(vs(iPoint))
            polygon.addPoint(point.x.toInt, point.y.toInt)
        }
        polygon
    }
    
    def drawLinesIfApplies(g2: SVGGraphics2D, edges: Array[Array[Int]], condition: (Int, Int) => Boolean, points: Array[Array[Double]]) {
        for (iArray <- edges) {
            val i1 = iArray(0)
            val i2 = iArray(1)
            if (condition(i1, i2)) {
                drawLine(g2, points(i1), points(i2))
            }
        }        
    }
    
    def drawLinesOn(g2: SVGGraphics2D, polyData: PolyData, matrix: Array[Array[Double]], visibleEdges: MHashSet[(Int, Int)], invisibleEdges: MHashSet[(Int, Int)]) {
        val transformedPoints = for (point <- polyData.points) yield transform(matrix, point)

        g2.setStroke(new BasicStroke(0.5f))
        g2.setPaint(new Color(0.2f, 0.2f, 0.2f))
        drawLinesIfApplies(g2, polyData.edges, (i1, i2) => !visibleEdges.contains((i1, i2)) && !invisibleEdges.contains((i1, i2)), transformedPoints)
        
        g2.setPaint(new Color(0.6f, 0.6f, 0.6f))
        g2.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f, Array(5.0f, 5.0f), 0.0f))
        drawLinesIfApplies(g2, polyData.edges, (i1, i2) => !visibleEdges.contains((i1, i2)) && invisibleEdges.contains((i1, i2)), transformedPoints)
        
        g2.setPaint(new Color(0.1f, 0.1f, 0.1f))
        g2.setStroke(new BasicStroke(1.5f))
        drawLinesIfApplies(g2, polyData.edges, (i1, i2) => visibleEdges.contains((i1, i2)), transformedPoints)
    }
    
    def isVisible(normal: Array[Double], matrix: Array[Array[Double]]) = normal match {
        case null => true
        case _ => {
            val nTransformed = transform(matrix, normal)
            nTransformed(2) > 0.0
        }
    }
    
    def drawFacesOn(g2: SVGGraphics2D, polyData: PolyData, matrix: Array[Array[Double]],
            visibleEdges: MHashSet[(Int, Int)], invisibleEdges: MHashSet[(Int, Int)], colorMap: Array[Color]) {
        val transformedPoints = for (p <- polyData.points) yield transform(matrix, p)
        val random = Random
        for ((face, iFace) <- polyData.faces.zipWithIndex) {
            if (isVisible(face.normal, matrix)) {
                g2.setColor(colorMap(iFace))
                val polygon = getPolygon(face.iPoints, transformedPoints)
                g2.fillPolygon(polygon)
                for (i <- 0 until face.iPoints.size) {
                    val iPoint1 = face.iPoints(i)
                    val iPoint2 = face.iPoints((i + 1) % face.iPoints.size)
                    visibleEdges.add((math.min(iPoint1, iPoint2), math.max(iPoint1, iPoint2)))
                }
            } else {
                for (i <- 0 until face.iPoints.size) {
                    val iPoint1 = face.iPoints(i)
                    val iPoint2 = face.iPoints((i + 1) % face.iPoints.size)
                    invisibleEdges.add((math.min(iPoint1, iPoint2), math.max(iPoint1, iPoint2)))
                }
            }
        }
    }
    
    def drawLines(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600)
        drawLinesOn(g2, info.polyData, info.matrix, MHashSet[(Int, Int)](), MHashSet[(Int, Int)]())
        g2.getSVGElement
    }
    
    def drawFaces(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600)
        val visibleEdges = MHashSet[(Int, Int)]()
        val invisibleEdges = MHashSet[(Int, Int)]()
        drawFacesOn(g2, info.polyData, info.matrix, visibleEdges, invisibleEdges, info.colorMap)
        g2.getSVGElement
    }
    
    def drawFacesAndLines(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600)
        val visibleEdges = MHashSet[(Int, Int)]()
        val invisibleEdges = MHashSet[(Int, Int)]()
        drawFacesOn(g2, info.polyData, info.matrix, visibleEdges, invisibleEdges, info.colorMap)
        drawLinesOn(g2, info.polyData, info.matrix, visibleEdges, invisibleEdges)
        g2.getSVGElement
    }
    
}