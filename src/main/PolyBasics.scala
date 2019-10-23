package main

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, HashSet => MHashSet}
import scala.runtime.ScalaRunTime
import scala.math.Ordering.Implicits._


case class Face(normal: Array[Double], iPoints: Vector[Int], name: String)

case class PolyData(points: Array[Array[Double]], edges: Array[Array[Int]], faces: Array[Face]) {
    def n = points(0).length
}
    
trait PolyConverter {
    def normalizePoints(points: Array[MyVec]) = {
        val n = points(0).p.length
        val pointsArray: Array[Array[Double]] = Array.fill(points.size)(null)
        var sum = MyVec(n)
        for (point <- points) {
            sum = sum + point
        }
        sum = sum * (-1.0 / points.size)
        var rMax = 0.0
        for (point <- points) {
            val r = (point + sum).length
            rMax = math.max(rMax, r)
        }
        for ((point, i) <- points.zipWithIndex) {
            pointsArray(i) = ((point + sum) * (1.0 / rMax)).p
        }
        pointsArray
    }
    
    def getPoints(pointMap: MHashMap[MyVec, Int]) = {
        val array = Array.fill[MyVec](pointMap.size)(null)
        for ((point, iPoint) <- pointMap) {
            array(iPoint) = point
        }
        normalizePoints(array)        
    }
    
    def getEdges(edgeMap: MHashSet[(Int, Int)]) = (for (edge <- edgeMap) yield Array(edge._1, edge._2)).toArray

    def getFaces(polyGeometry: PolyGeometry): Array[Face]
    
    def apply(polyGeometry: PolyGeometry) = PolyData(
            getPoints(polyGeometry.pointMap),
            getEdges(polyGeometry.edgeMap),
            getFaces(polyGeometry))   
    
}


object PolyConverter3D extends PolyConverter {

    def getFace(edges: MHashSet[(Int, Int)], iPointBuffer: Array[Array[Int]]) = {
        for ((i1, i2) <- edges) {
            for (i <- 0 until 2) {
                iPointBuffer(i1)(i) = -1
                iPointBuffer(i2)(i) = -1
            }
        }
        var i0 = -1
        for ((i1, i2) <- edges) {
            if (iPointBuffer(i1)(0) == -1) {
                iPointBuffer(i1)(0) = i2
            } else {
                iPointBuffer(i1)(1) = i2
            }
            if (iPointBuffer(i2)(0) == -1) {
                iPointBuffer(i2)(0) = i1
            } else {
                iPointBuffer(i2)(1) = i1
            }
            i0 = i1
        }
        val iPoints = ArrayBuffer[Int]()
        var i = i0
        var iNext = 0
        var iPrev = -1
        do {
            iNext = iPointBuffer(i)(1)
            if (iNext == iPrev && iNext != -1) {
                iNext = iPointBuffer(i)(0)
            }
            iPrev = i
            i = iNext
        } while (i != i0 && iNext != -1)

        i0 = iPrev
        iPrev = -1
        i = i0
        do {
            var iNext = iPointBuffer(i)(0)
            if (iNext == iPrev) {
                iNext = iPointBuffer(i)(1)
            }
            if (iPrev == -1 || i != i0) {
                iPoints.append(i)
            }
            iPrev = i
            i = iNext
        } while (i != i0 && i != -1)
        iPoints.toVector
    }
    
    override def getFaces(polyGeometry: PolyGeometry) = {
        val iPointBuffer = Array.ofDim[Int](polyGeometry.pointMap.size, 2)
        val foundFaces = MHashMap[MyVec, Vector[Int]]()
        for ((normal, edges) <- polyGeometry.faceMap) {
            foundFaces(normal) = getFace(edges, iPointBuffer);
        }
        (for ((normal, iPoints) <- foundFaces.toSeq.sortBy(entry => entry._2))
            yield Face(normal.p, iPoints, polyGeometry.faceTypeMap(normal))).toArray
    }
    
}

