object Day2b {

  def main(args: Array[String]): Unit = {
    val input = Day2a.parseInput(args(0))
    println(searchForAnswer(input))
  }

  val MAGIC_NUM = 19690720

  def searchForAnswer(input: Array[Int]): Int = {
    var output = -1
    Range(0, 100).foreach { noun =>
      Range(0, 100).foreach { verb =>
        if (test(input.clone(), noun, verb)) {
          output = (100 * noun + verb)
          return output
        }
      }
    }
    output
  }

  def test(input: Array[Int], noun: Int, verb: Int): Boolean = {
    input(1) = noun
    input(2) = verb
    Day2a.process(input) == MAGIC_NUM
  }
}
