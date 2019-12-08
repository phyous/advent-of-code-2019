class Day4aTest extends org.scalatest.FunSuite {

  test ("intSplat") {
    val ret = Day4a.intSplat(1234)
    assert(ret == List(1,2,3,4))
  }
}
