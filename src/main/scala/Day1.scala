import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    println(parseInput(args(0)).map(computeFuel(_)).sum)
  }

  def parseInput(file: String):List[Int] = {
    Source.fromFile(file).getLines.toList.map(_.toInt)
  }

  def computeFuel(mass: Int, startFuel: Int = 0): Int = {
    val compute = (mass / 3) - 2
    val additionalFuel =  if (compute <= 0) 0 else compute
    if (additionalFuel == 0) {
      startFuel
    } else {
      computeFuel(additionalFuel, startFuel + additionalFuel)
    }
  }

}
