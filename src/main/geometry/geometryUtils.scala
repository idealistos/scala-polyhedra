package main.geometry

case class Face(normal: Array[Double], iPoints: Vector[Int], name: String)

case class Angle(iPoint: Int, point1: MyVec, point2: MyVec, name: String)