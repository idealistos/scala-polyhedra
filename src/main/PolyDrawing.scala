package main

import java.awt.Color
import java.awt.geom.Line2D
import java.awt.geom.Point2D

import scala.util.Random
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, HashSet => MHashSet}

import org.jfree.graphics2d.svg.SVGGraphics2D
import java.awt.Stroke
import java.awt.BasicStroke

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

    def getRandomRotationMatrix(n: Int) = {
        val m = (for (i <- 0 until n) yield MyVec(n, i).p).toArray
        val random = Random
        for (i <- 0 until 100) {
            val i1 = random.nextInt.abs % n
            val i2 = ((random.nextInt.abs % (n - 1) + 1) + i1) % n
            rotateRandomly(m, i1, i2, random.nextDouble)
        }
        m
    }

    def apply(polyData: PolyData) = {
        val names = MHashSet[String]()
        for (face <- polyData.faces) {
            names.add(face.name)
        }
        val colorByName = MHashMap[String, Color]() ++ (for (name <- names) yield (name -> getRandomColor))
        val colorMap = for (face <- polyData.faces) yield colorByName(face.name)
        new PolyDrawingInfo(polyData, getRandomRotationMatrix(polyData.n), colorMap)
    }
    
}

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
    
    def drawLinesOn(g2: SVGGraphics2D, polyData: PolyData, matrix: Array[Array[Double]], visibleEdges: MHashSet[(Int, Int)]) {
        val transformedPoints = for (point <- polyData.points) yield transform(matrix, point)
        
        g2.setStroke(new BasicStroke(1.0f))
        if (visibleEdges != null) {
            g2.setPaint(new Color(0.6f, 0.6f, 0.6f))
            g2.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f, Array(5.0f, 5.0f), 0.0f))
            for (iArray <- polyData.edges) {
                val i1 = iArray(0)
                val i2 = iArray(1)
                if (!visibleEdges.contains((i1, i2))) {
                    drawLine(g2, transformedPoints(i1), transformedPoints(i2))
                }
            }
        }
        g2.setPaint(new Color(0.1f, 0.1f, 0.1f))
        g2.setStroke(new BasicStroke(1.0f))
        for (iArray <- polyData.edges) {
            val i1 = iArray(0)
            val i2 = iArray(1)
            if (visibleEdges == null || visibleEdges.contains((i1, i2))) {
                drawLine(g2, transformedPoints(i1), transformedPoints(i2))
            }
        }
    }
    
    def drawFacesOn(g2: SVGGraphics2D, polyData: PolyData, matrix: Array[Array[Double]],
            visibleEdges: MHashSet[(Int, Int)], colorMap: Array[Color]) {
        val transformedPoints = for (p <- polyData.points) yield transform(matrix, p)
        val random = Random
        for ((face, iFace) <- polyData.faces.zipWithIndex) {
            val nTransformed = transform(matrix, face.normal)
            if (nTransformed(2) > 0.0) {
                g2.setColor(colorMap(iFace))
                val polygon = getPolygon(face.iPoints, transformedPoints)
                g2.fillPolygon(polygon)
                for (i <- 0 until face.iPoints.size) {
                    val iPoint1 = face.iPoints(i)
                    val iPoint2 = face.iPoints((i + 1) % face.iPoints.size)
                    visibleEdges.add((math.min(iPoint1, iPoint2), math.max(iPoint1, iPoint2)))
                }
            }
        }
    }
    
    def drawLines(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600);
        drawLinesOn(g2, info.polyData, info.matrix, null)
        g2.getSVGElement
    }
    
    def drawFaces(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600);
        val visibleEdges = MHashSet[(Int, Int)]()
        drawFacesOn(g2, info.polyData, info.matrix, visibleEdges, info.colorMap)
        g2.getSVGElement
    }
    
    def drawFacesAndLines(info: PolyDrawingInfo) = {
        val g2 = new SVGGraphics2D(1000, 600);
        val visibleEdges = MHashSet[(Int, Int)]()
        drawFacesOn(g2, info.polyData, info.matrix, visibleEdges, info.colorMap)
        drawLinesOn(g2, info.polyData, info.matrix, visibleEdges)
        g2.getSVGElement
    }
    
}