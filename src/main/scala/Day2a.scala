import scala.io.Source

object Day2a {

  def main(args: Array[String]): Unit = {
    val input: Array[Int] = parseInput(args(0))

    input(1) = 12
    input(2) = 2
    println(process(input))
  }

  def parseInput(inputStr: String): Array[Int] = {
    Source.fromFile(inputStr).getLines.next().split(",").map(x => x.toInt)
  }

  def process(input: Array[Int]): Int = {
    var currentPos = 0
    while (input(currentPos) != 99) {
      val opCode = input(currentPos)
      val a = input(currentPos + 1)
      val b = input(currentPos + 2)
      val storeLocation = input(currentPos + 3)

      input(storeLocation) = opCode match {
        case 1 => input(a) + input(b)
        case 2 => input(a) * input(b)
      }
      currentPos += 4
    }

    input(0)
  }
}
