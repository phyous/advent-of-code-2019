import Day3a.{Point, Vector, drawWire}

import scala.collection.mutable
import scala.io.Source

object Day3b {

  case class Vector(direction: String, magnitude: Int)

  case class Point(x: Int, y: Int, len: Int)

  def main(args: Array[String]): Unit = {
    val input = parseInput(args(0))
    println(input)
    val minDistance = detectMinCross(input)
    println(minDistance)
  }

  def parseInput(inputFile: String): List[List[Vector]] = {
    val lines = Source.fromFile(inputFile).getLines.toList
    lines
      .map { x =>
        x
          .split(",")
          .toList
          .map(y => Vector(y.charAt(0).toString, y.substring(1).toInt))
      }
  }

  def detectMinCross(input: List[List[Vector]]): Int = {
    // Generate two sets representing unique points in each wire
    val ret: List[Set[Point]] = input
      .map { wire => // For each wire
        var curPoint = Point(0, 0, 0)
        var pointSet = Set[Point]()
        wire
          .foreach { vector => // Draw each vector
            val r = drawWire(curPoint, vector, pointSet);
            curPoint = r._2
            pointSet = r._1
            println(curPoint)
          }
        pointSet
      }
      .map(removeDuplicates)


    // Find the intersection and take the min distance
    val map1 = ret.head.map(p => (p.x, p.y) -> p.len).toMap
    val intersectionPoints = ret(1).filter(p => map1.get((p.x, p.y)).isDefined)
    intersectionPoints.map(p => map1.getOrElse((p.x, p.y), Int.MaxValue) + p.len).min
  }

  def removeDuplicates(pointSet: Set[Point]): Set[Point] = {
    pointSet
      .map(p => ((p.x, p.y), p.len))
      .groupBy(_._1)
      .map { ps => Point(ps._1._1, ps._1._2, ps._2.minBy(_._2)._2) }
      .toSet
  }

  def drawWire(curPoint: Point, vector: Vector, pointSet: Set[Point]): (Set[Point], Point) = {
    var lastPoint = curPoint
    val newPointSet = pointSet ++ (1 to vector.magnitude).map { i =>
      lastPoint = nextPoint(curPoint, i, vector.direction)
      lastPoint
    }

    (newPointSet, lastPoint)
  }

  def nextPoint(lastPoint: Point, movementAmount: Int, direction: String): Point = {
    direction match {
      case "U" => Point(lastPoint.x, lastPoint.y + movementAmount, lastPoint.len + movementAmount)
      case "D" => Point(lastPoint.x, lastPoint.y - movementAmount, lastPoint.len + movementAmount)
      case "R" => Point(lastPoint.x + movementAmount, lastPoint.y, lastPoint.len + movementAmount)
      case "L" => Point(lastPoint.x - movementAmount, lastPoint.y, lastPoint.len + movementAmount)
    }
  }
}
