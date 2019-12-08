import Day4a.{PasswordData, increasing, parseInput}

object Day4b {

  def main(args: Array[String]): Unit = {
    val passwordData = Day4a.parseInput(args(0))
    val ret = searchForAnswer(passwordData)
    println(ret)
    println(ret.length)
  }

  def searchForAnswer(passwordData: PasswordData): List[Int] = {
    (passwordData.lowerRange to passwordData.upperRange)
      .filter(passesCriteria)
      .toList
  }

  def passesCriteria(input: Int): Boolean = {
    val intList = intSplat(input)
    hasTwoConsecutiveNotPartOfGroup(intList) && increasing(intList)
  }

  def intSplat(input: Int): List[Int] = {
    input.toString.split("").toList.map(_.toInt)
  }

  def hasTwoConsecutiveNotPartOfGroup(input: List[Int]): Boolean = {
    val groups = parseConsecutive(input)
    groups.exists(_.length == 2)
  }

  @scala.annotation.tailrec
  def increasing(input: List[Int]): Boolean = {
    input match {
      case Nil => true
      case head :: Nil => true
      case head :: tail => (head <= tail.head) && increasing(tail)
    }
  }

  @scala.annotation.tailrec
  def parseConsecutive(input: List[Int],
                       consecutive: List[Int] = List(),
                       ret: List[List[Int]] = List()): List[List[Int]] = {
    input match {
      case Nil => ret
      case head :: Nil =>
        if (consecutive != Nil && head == consecutive.head) {
          (head :: consecutive) :: ret
        } else {
          List(head) :: ret
        }
      case head :: tail =>
        if (head == tail.head) {
          parseConsecutive(tail, head :: consecutive, ret)
        } else {
          parseConsecutive(tail, List[Int](), (head :: consecutive) :: ret)
        }
    }
  }

}
