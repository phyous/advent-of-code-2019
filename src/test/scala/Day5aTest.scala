import Day5a.Instruction

class Day5aTest extends org.scalatest.FunSuite {

  test ("parseOpcode") {
    assert(Day5a.parseOpcode(1002) == (2, List(0,1,0)))
    assert(Day5a.parseOpcode(11102) == (2, List(1,1,1)))
    assert(Day5a.parseOpcode(3) == (3, List(0)))
  }

  test ("parseInstruction") {
    assert(
      Day5a.parseInstruction(Array(1002,4,3,4,33), 0) ==
      Instruction(opCode = 2, positionModes = List(0,1,0), params = List(4, 3, 4), length = 4)
    )

    assert(
      Day5a.parseInstruction(Array(103,4,3,4,33), 0) ==
        Instruction(opCode = 3, positionModes = List(1), params = List(4), length = 2)
    )
  }

}
