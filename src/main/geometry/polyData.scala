package main.geometry

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import scala.runtime.ScalaRunTime
import scala.math.Ordering.Implicits._
import scala.collection.immutable.HashSet
import main.value.Implicits._

abstract class PolyData {
    val points: Array[Array[Double]]
    val edges: Array[Array[Int]]
    val faces: Array[Face]
    def n = points(0).length

    def normalizePoints(points: Array[MyVec], iPoints: Iterable[Int]): Array[MyVec] = {
        val n = points(0).p.length
        val pointsArray: Array[MyVec] = Array.fill(points.size)(null)
        var sum = MyVec(n)
        for (iPoint <- iPoints) {
            sum = sum + points(iPoint)
        }
        sum = sum * (-1.0 / iPoints.size)
        var rMax = 0.0
        for (iPoint <- iPoints) {
            val r = (points(iPoint) + sum).length
            rMax = math.max(rMax, r)
        }
        for ((point, i) <- points.zipWithIndex) {
            pointsArray(i) = ((point + sum) * (1.0 / rMax))
        }
        pointsArray
    }
    
    def normalizePoints(points: Array[MyVec]): Array[MyVec] = normalizePoints(points, points.indices)

    def needsRotationMatrix = true
    
}

object PolyData {
    def apply(polyGeometry: PolyGeometry) = polyGeometry match {
        case g: ConvexPolyGeometry if g.n == 3 => PolyData3D(g)
        case g: ConvexPolyGeometry if g.n == 4 => StereoPolyData4D(g)
        case g: TilingPolyGeometry if g.n == 3 => TilingPolyData3D(g)
        case _ => throw new Exception("Cannot create poly data")
    }

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
                iPoints += i
            }
            iPrev = i
            i = iNext
        } while (i != i0 && i != -1)
        iPoints.toVector
    }

}

case class PolyData3D(polyGeometry: ConvexPolyGeometry) extends PolyData {
    
    val points = normalizePoints(polyGeometry.points).map(_.p)
    
    val edges = polyGeometry.edges
    
    val faces = {
        val iPointBuffer = Array.ofDim[Int](polyGeometry.pointMap.size, 2)
        val foundFaces = MHashMap[MyVec, Vector[Int]]()
        for ((normal, edges) <- polyGeometry.faceMap) {
            foundFaces(normal) = PolyData.getFace(edges, iPointBuffer)
        }
        (for ((normal, iPoints) <- foundFaces.toSeq.sortBy(entry => entry._2))
            yield Face(normal.p, iPoints, polyGeometry.faceTypeMap(normal))).toArray
    }
    
}

case class StereoPolyData4D(polyGeometry: ConvexPolyGeometry) extends PolyData {
    
    val maxEdgeCountOfFace = polyGeometry.faceMap.values.map(_.size).fold(0)(math.max)
    val faceNormal = polyGeometry.faceMap.find(x => x._2.size == maxEdgeCountOfFace).get._1
    val faceVertices = polyGeometry.faceMap(faceNormal).foldLeft(HashSet[Int]())((set, edge) => set + edge._1 + edge._2)
    
    val zNormal = MyVec(Array(0.0, 0.0, 0.0, 1.0))
    // Projection: a' = a - 2 (a . p) p / |p|^2
    val vProjection = if ((faceNormal - zNormal).length < Tolerance) MyVec(4) else (faceNormal - zNormal).normalize
    
    def project(point: MyVec) = point - vProjection * (2.0 * (vProjection ^ point))
    
    val projectedPoints = polyGeometry.points.map(project)
    val normalizedPoints = normalizePoints(projectedPoints, faceVertices)
    val dt = -0.25
    val nStereo = MyVec(Array(0.0, 0.0, 0.0, dt))
    
    def stereo(point: MyVec) = point * (dt / (dt - point.p(3))) - nStereo * (point.p(3) / (dt - point.p(3)))
    
    val points = normalizedPoints.map(stereo).map(_.p.take(3))
    val edges = polyGeometry.edges
    
    val faces = {
        val subFaceTypes = polyGeometry.subFaceTypeMap(faceNormal).toArray
        val projectedSubNormals = subFaceTypes.map(_._1).map(project).map(_.p.take(3))
        val iPointBuffer = Array.ofDim[Int](polyGeometry.pointMap.size, 2)
        for ((subNormal, subFaceType) <- polyGeometry.subFaceTypeMap(faceNormal).toArray) yield {
            val projectedSubNormal = project(subNormal).p.take(3)
            val edges = polyGeometry.subFaceMap(faceNormal)(subNormal)
            val iPoints = PolyData.getFace(edges, iPointBuffer)
            Face(projectedSubNormal, iPoints, subFaceType)
        }
    }
    
}

case class TilingPolyData3D(polyGeometry: TilingPolyGeometry) extends PolyData {
    val points = normalizePoints(polyGeometry.points).map(_.p)
    val edges = polyGeometry.edges
    
    def getNextNbMap = {
        // (i1, i2) -> i3, if there is some face where i1, i2, i3 are consecutive vertices in counter-clockwise order
        val nbMap = MHashMap[(Int, Int), (Int, String)]()
        val pointMap = polyGeometry.pointMap.withDefaultValue(-1)
        for (faceAngle <- polyGeometry.faceAngleMap) {
            val point0 = polyGeometry.points(faceAngle.iPoint)
            val i1 = pointMap(faceAngle.point1)
            val i2 = pointMap(faceAngle.point2)
            if (i1 >= 0 && i2 >= 0) {
                val shift1 = faceAngle.point1 - point0
                val shift2 = faceAngle.point2 - point0
                val order = shift1.planarCrossProduct(shift2) >= 0.0
                if (order) {
                    nbMap((i2, faceAngle.iPoint)) = (i1, faceAngle.name)
                } else {
                    nbMap((i1, faceAngle.iPoint)) = (i2, faceAngle.name)
                }
            }
        }
        nbMap
    }
    
    val faces = {
        val nbMap = getNextNbMap
        val foundFaces = ArrayBuffer[Face]()
        val visitedEdges = MHashSet[(Int, Int)]()
        
        def formFace(iPoint1: Int, iPoint2: Int) = {
            var i1 = iPoint1
            var i2 = iPoint2
            val iPoints = ArrayBuffer[Int](iPoint2)
            var face: Option[Face] = None
            var canContinue = true
            var faceName: String = null
            while (canContinue && !visitedEdges.contains(i1, i2)) {
                nbMap.get((i1, i2)) match {
                    case None => canContinue = false
                    case Some((iNew, name)) => {
                        if (faceName == null) {
                            faceName = name
                        } else if (faceName != name) {
                            throw new Exception("Face name mismatch")
                        }
                        iPoints += iNew
                        visitedEdges += i1 -> i2
                        if (iNew == iPoint1) {
                            visitedEdges += i2 -> iNew
                            face = Some(Face(null, iPoints.toVector, faceName))
                            canContinue = false
                        } else {
                            i1 = i2
                            i2 = iNew
                        }
                    }
                }
            }
            face
        }
        
        for (Array(iPoint1, iPoint2) <- polyGeometry.edges) {
            val edge = (iPoint1, iPoint2)
            if (!visitedEdges.contains(edge)) {
                formFace(iPoint1, iPoint2) match {
                    case None => 
                    case Some(face) => foundFaces += face
                }
                
            }
        }
        foundFaces.toArray
    }
    
    override def needsRotationMatrix = false
}