import scala.collection.mutable
import scala.io.Source

object Day3a {
  case class Vector(direction: String, magnitude: Int)
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val input = parseInput(args(0))
    val crosses = detectCrosses(input)
    println(crosses)
    val closestCross = findClosest(Point(0,0), crosses)
    println(closestCross.x+closestCross.y)
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

  def detectCrosses(input: List[List[Vector]]): List[Point] = {
    val pointMap = mutable.Map[Point, mutable.Set[Int]]()
    var wireNum = 0

    input
      .map { wire => // For each wire
        var curPoint = Point(0, 0)
        wireNum += 1
        wire
          .map { vector => // Draw each vector
            curPoint = drawWire(curPoint, vector, pointMap, wireNum)
            println(curPoint)
          }
      }

    // Select only points with a cross
    pointMap.toList.filter(p => p._2.size == 2).map(p => p._1)
  }

  def drawWire(curPoint: Point, vector: Vector, pointMap: mutable.Map[Point, mutable.Set[Int]], wireNum: Int): Point = {
    var lastPoint = curPoint
    vector.direction match {
      case "U" =>
        (1 to vector.magnitude)
          .map(i => Point(curPoint.x, curPoint.y + i))
          .foreach { point =>
            lastPoint = point
            pointMap.updateWith(point)(curVal => Some(curVal.getOrElse(mutable.Set()).addOne(wireNum)))
          }
      case "D" =>
        (1 to vector.magnitude)
          .map(i => Point(curPoint.x, curPoint.y - i))
          .foreach { point =>
            lastPoint = point
            pointMap.updateWith(point)(curVal => Some(curVal.getOrElse(mutable.Set()).addOne(wireNum)))
          }
      case "L" =>
        (1 to vector.magnitude)
          .map(i => Point(curPoint.x - i, curPoint.y))
          .foreach { point =>
            lastPoint = point
            pointMap.updateWith(point)(curVal => Some(curVal.getOrElse(mutable.Set()).addOne(wireNum)))
          }
      case "R" =>
        (1 to vector.magnitude)
          .map(i => Point(curPoint.x + i, curPoint.y))
          .foreach { point =>
            lastPoint = point
            pointMap.updateWith(point)(curVal => Some(curVal.getOrElse(mutable.Set()).addOne(wireNum)))
          }
    }
    lastPoint
  }

  def findClosest(origin: Point, candidates: List[Point]): Point = {
    case class TP(point: Point, dist: Int)
    candidates.map{p => TP(p, distance(origin, p))}.minBy(p => p.dist).point
  }

  def distance(p1: Point, p2:Point): Int = {
    math.abs(p1.x-p2.x) + math.abs(p1.y - p2.y)
  }
}