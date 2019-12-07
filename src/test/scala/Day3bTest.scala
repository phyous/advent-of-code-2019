import Day3b.{Point, Vector}
import org.scalatest._

class Day3bTest extends org.scalatest.FunSuite {

  test ("drawWire") {
    val curPoint = Point(0, 0, 0)
    val vector = Vector("R", 8)
    var pointSet = Set[Point]()

    val ret = Day3b.drawWire(curPoint, vector, pointSet)
    val expected = Set(Point(1,0,1), Point(4,0,1), Point(3,0,1), Point(5,0,1), Point(2,0,1), Point(6,0,1), Point(7,0,1), Point(8,0,1))
    assert(ret._1 == expected)
  }
}
