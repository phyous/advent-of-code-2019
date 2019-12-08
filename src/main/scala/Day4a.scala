object Day4a {

  case class PasswordData(lowerRange: Int, upperRange: Int)

  def main(args: Array[String]): Unit = {
    val passwordData = parseInput(args(0))
    val answers = searchForAnswer(passwordData)
    println(answers)
    println(answers.length)
  }

  def parseInput(inputStr: String): PasswordData = {
    val splits = inputStr.split("-")
    PasswordData(splits(0).toInt, splits(1).toInt)
  }

  def searchForAnswer(passwordData: PasswordData): List[Int] = {
    (passwordData.lowerRange to passwordData.upperRange)
      .filter(passesCriteria)
      .toList
  }

  def passesCriteria(input: Int): Boolean = {
    val intList = intSplat(input)
    hasTwoConsecutive(intList) && increasing(intList)
  }

  def intSplat(input: Int): List[Int] = {
    input.toString.split("").toList.map(_.toInt)
  }

  @scala.annotation.tailrec
  def hasTwoConsecutive(input: List[Int]): Boolean = {
    input match {
      case Nil => false
      case head :: Nil => false
      case head :: tail => (head == tail.head) || hasTwoConsecutive(tail)
    }
  }

  @scala.annotation.tailrec
  def increasing(input: List[Int]): Boolean = {
    input match {
      case Nil => true
      case head :: Nil => true
      case head :: tail => (head <= tail.head) && increasing(tail)
    }
  }

}
