package main.geometry

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet, TreeMap => MTreeMap }
import main.poly.Poly
import main.value.Implicits._

trait PolyGeometry {
    def pointMap: MHashMap[MyVec, Int]
    def edgeMap: MHashSet[(Int, Int)]
    def n: Int
    def polyClassName: String
    
    lazy val points = {
        val points = Array.fill[MyVec](pointMap.size)(null)
        for ((point, iPoint) <- pointMap) {
            points(iPoint) = point
        }
        points
    }
    
    lazy val edges = (for (edge <- edgeMap) yield Array(edge._1, edge._2)).toArray
}

trait ConvexPolyGeometry extends PolyGeometry {
    def faceMap: MHashMap[MyVec, MHashSet[(Int, Int)]]
    def faceTypeMap: MHashMap[MyVec, String]
    def subFaceMap: MHashMap[MyVec, MHashMap[MyVec, MHashSet[(Int, Int)]]] // face normal x sub-face normal -> edges
    def subFaceTypeMap: MHashMap[MyVec, MHashMap[MyVec, String]]
}

trait TilingPolyGeometry extends PolyGeometry {
    def faceAngleMap: MHashSet[Angle]
}

object PolyGeometry {
    def apply(poly: Poly) = {
        if (poly.isTiling) {
            ComputedTilingPolyGeometry(poly).calculate()
        } else {
            ComputedConvexPolyGeometry(poly).calculate()
        }
    }
}

abstract class ComputedPolyGeometry(poly: Poly) extends PolyGeometry {
    val pointMap = MHashMap[MyVec, Int]()
    val edgeMap = MHashSet[(Int, Int)]()
    val n = poly.vList(0).rank
    
    def registerPoint(point: MyVec) = pointMap.getOrElseUpdate(point, pointMap.size)
    def registerEdge(bundle: Bundle, iPoint1: Int, iPoint2: Int): Unit
    def registerBundleElements(iShift: Int, bundle: Bundle, newBundle: Bundle): Unit
    
    def registerEdge(iPoint1: Int, iPoint2: Int) {
        if (iPoint1 < iPoint2) {
            edgeMap.add((iPoint1, iPoint2))
        }
    }
    
    def checkMaxQueueSize(i: Int) = true

    def calculate(): this.type = {
        val queue = ArrayBuffer[Bundle]()
        val visited = MTreeMap[Double, Bundle]()
        var hits = 0
        var misses = 0
        
        def register(bundle: Bundle) {
            val projection = bundle.projections(0)
            val same = visited.rangeImpl(Some(projection - 10.0 * Tolerance), Some(projection + 10.0 * Tolerance)).values
                    .find(_ == bundle)
            same match {
                case None => {
                    queue += bundle
                    visited += projection -> bundle
                    misses += 1
                }
                case _ => hits += 1
            }
        }
        
        register(Bundle(poly))
        registerPoint(queue(0).basis(0))
        var i = 0
        while (i < queue.size && checkMaxQueueSize(i)) {
            val bundle = queue(i)
            for (j <- 0 until n) {
                val newBundle = bundle.shift(j)
                register(newBundle)
                if (j == 0) {
                    val iOldPoint = pointMap(bundle.basis(0))
                    val iNewPoint = registerPoint(newBundle.basis(0))
                    registerEdge(newBundle, iOldPoint, iNewPoint)
                }
                registerBundleElements(j, bundle, newBundle)
            }
            i += 1
            if (i % 500 == 0) {
                println(s"Hits: $hits, misses: $misses, index: $i, queue size: ${queue.size}")
            }
        }
        println("Geometry calculated")
        this
    }
    
    override def polyClassName = poly.getClass.getSimpleName
    
}

case class ComputedConvexPolyGeometry(poly: Poly) extends ComputedPolyGeometry(poly) with ConvexPolyGeometry {
    val faceMap = MHashMap[MyVec, MHashSet[(Int, Int)]]()
    val faceTypeMap = MHashMap[MyVec, String]()
    val subFaceMap = MHashMap[MyVec, MHashMap[MyVec, MHashSet[(Int, Int)]]]() // face normal x sub-face normal -> edges
    val subFaceTypeMap = MHashMap[MyVec, MHashMap[MyVec, String]]()

    def registerFaceEdge(normal: MyVec, iPoint1: Int, iPoint2: Int) {
        val iPointMin = math.min(iPoint1, iPoint2)
        val iPointMax = math.max(iPoint1, iPoint2)
        val edges = faceMap.getOrElseUpdate(normal, MHashSet())
        edges.add((iPointMin, iPointMax))
    }
    
    def registerFaceType(normal: MyVec, faceName: String) {
        faceTypeMap(normal) = faceName
    }
    
    def registerSubFaceEdge(normal: MyVec, subNormal: MyVec, iPoint1: Int, iPoint2: Int) {
        val iPointMin = math.min(iPoint1, iPoint2)
        val iPointMax = math.max(iPoint1, iPoint2)
        val edges = subFaceMap.getOrElseUpdate(normal, MHashMap()).getOrElseUpdate(subNormal, MHashSet())
        edges.add((iPointMin, iPointMax))
    }
        
    def registerSubFaceType(normal: MyVec, subNormal: MyVec, subFaceName: String) {
        subFaceTypeMap.getOrElseUpdate(normal, MHashMap())(subNormal) = subFaceName
    }
    
    override def registerEdge(newBundle: Bundle, iPoint1: Int, iPoint2: Int) {
        registerEdge(iPoint1, iPoint2)
        registerFaceEdge(newBundle.basis(n), iPoint1, iPoint2)
        if (n >= 4) {
            registerSubFaceEdge(newBundle.basis(n), newBundle.basis(n - 1), iPoint1, iPoint2)
        }
    }
    
    override def registerBundleElements(iShift: Int, bundle: Bundle, newBundle: Bundle) {
        if (iShift == n - 1) {
            registerFaceType(newBundle.basis(n), newBundle.v.getFaceParent.name)
        }
        if (n >= 4 && iShift == n - 2) {
            registerSubFaceType(newBundle.basis(n), newBundle.basis(n - 1), newBundle.v.getSubFaceParent.name)
        }
    }

}

case class ComputedTilingPolyGeometry(poly: Poly) extends ComputedPolyGeometry(poly) with TilingPolyGeometry {
    
    val faceAngleMap = MHashSet[Angle]()
    
    override def registerEdge(newBundle: Bundle, iPoint1: Int, iPoint2: Int) {
        registerEdge(iPoint1, iPoint2)
    }
    
    override def registerBundleElements(iShift: Int, bundle: Bundle, newBundle: Bundle) {
        if (iShift == 1) {
            val iPoint = pointMap(bundle.basis(0))
            val angle = Angle(iPoint, bundle.shiftedVertex, newBundle.shiftedVertex, newBundle.v.getFaceParent.name)
            faceAngleMap += angle
        }
    }
    
    override def checkMaxQueueSize(i: Int) = i < 1000
    
}