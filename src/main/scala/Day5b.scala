import scala.io.Source
import util.control.Breaks._
import scala.io.StdIn.readInt

object Day5b {

  def main(args: Array[String]): Unit = {
    val input = parseInput(args(0))
    process(input)
  }

  def parseInput(inputStr: String): Array[Int] = {
    Source.fromFile(inputStr).getLines.next().split(",").map(x => x.toInt)
  }

  def process(input: Array[Int]): Unit = {
    var currentPos = 0

    breakable  {
      while (true) {
        val instruction = parseInstruction(input, currentPos)
        println(s"Processing: ${instruction}")
        if (instruction.opCode == 99) break
        currentPos = processInstruction(instruction, input, currentPos)
      }
    }
  }

  def getProcessedParams(instruction: Instruction, input: Array[Int]):List[Int] = {
    val params = (instruction.params zip instruction.positionModes)
    params.map{case (param, mode) => mode match {case 0 => input(param); case 1 => param}}
  }

  def processInstruction(instruction: Instruction, input: Array[Int], instructionPos: Int): Int = {
    instruction.opCode match {
      case op if op <= 2 =>
        val processedParams = getProcessedParams(instruction, input)
        val outputAddress = instruction.params(2)
        op match  {
          case 1 => input(outputAddress) = processedParams(0) + processedParams(1)
          case 2 => input(outputAddress) = processedParams(0) * processedParams(1)
        }
        instructionPos + instruction.length
      case 3 =>
        print("Enter input: ")
        val consoleIn = readInt()
        input(instruction.params(0)) = consoleIn
        instructionPos + instruction.length
      case 4 =>
        instruction.positionModes.head match {
          case 0 => println(s"OUTPUT COMMAND 4: ${input(instruction.params.head)}")
          case 1 => println(s"OUTPUT COMMAND 4: ${instruction.params.head}")
        }
        instructionPos + instruction.length
      case 5 =>
        val processedParams = getProcessedParams(instruction, input)
        if (processedParams.head != 0) processedParams(1)
        else instructionPos + instruction.length
      case 6 =>
        val processedParams = getProcessedParams(instruction, input)
        if (processedParams.head == 0) processedParams(1)
        else instructionPos + instruction.length
      case 7 =>
        val processedParams = getProcessedParams(instruction, input)
        if (processedParams(0) < processedParams(1)) input(instruction.params(2)) = 1
        else input(instruction.params(2)) = 0
        instructionPos + instruction.length
      case 8 =>
        val processedParams = getProcessedParams(instruction, input)
        if (processedParams(0) == processedParams(1)) input(instruction.params(2)) = 1
        else input(instruction.params(2)) = 0
        instructionPos + instruction.length
    }
  }

  case class Instruction(opCode: Int, positionModes: List[Int], params: List[Int], length: Int)
  val INSTRUCTION_LENGTHS = Map(1 -> 4, 2 -> 4, 3 -> 2, 4 -> 2, 5 -> 3, 6 -> 3, 7 -> 4, 8 -> 4, 9 -> 1, 99 -> 1)

  def parseInstruction(input: Array[Int], pos: Int): Instruction = {
    val parsedOpcode = parseOpcode(input(pos))
    val opCode = parsedOpcode._1
    val positionModes = parsedOpcode._2
    val length = INSTRUCTION_LENGTHS.getOrElse(opCode, Int.MaxValue)
    val params = input.slice(pos + 1, pos + length).toList

    Instruction(opCode, positionModes, params, length)
  }

  def parseOpcode(input: Int): (Int, List[Int]) = {
    val ints = input
      .toString
      .split("")
      .map(_.toInt)

    // Pad opcode so we can treat instructions the same
    val requiredLength = INSTRUCTION_LENGTHS.getOrElse(ints.last, Int.MaxValue) + 1
    val actualLength = ints.length
    val padSize = requiredLength - actualLength
    assert(padSize >= 0, "we should only calculate a non negative pad size")
    val paded = ints.prependedAll((0 until padSize).map(_ => 0))

    val opCode = 10 * paded(paded.length - 2) + paded(paded.length - 1)
    val modes = paded.slice(0, paded.length - 2).toList.reverse
    (opCode, modes)
  }
}
