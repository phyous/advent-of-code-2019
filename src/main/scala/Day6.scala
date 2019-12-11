import scala.io.Source

object Day6 {

  def main(args: Array[String]): Unit = {
    val nodes = parseInput(Source.fromFile(args(0)).getLines.toList)
    println(nodes)
    val checksum = computeChecksum(nodes)
    println(s"checksum: ${checksum}")
    val dist = computeOrbitalDistance(nodes)
    println(s"dist: ${dist}")
  }

  case class Node(name: String, parent: Option[String], children: List[String])

  def parseInput(input: List[String]): Map[String, Node] = {
    // Pointers from parent to child
    var childrenPointers = Map[String, List[String]]()
    // Pointers from child back up to parent
    var parentPointers = Map[String, String]()
    // Unique set of all bodies
    var nodes = Set[String]()

    input
      .foreach { n =>
        val splits = n.split("\\)")
        val parent = splits(0)
        val child = splits(1)
        val existingChildren = childrenPointers.get(parent)
        childrenPointers += (parent -> (child :: existingChildren.getOrElse(Nil)))
        parentPointers += (child -> parent)
        nodes += parent
        nodes += child
      }

    nodes
      .map(n => (n -> Node(n, parentPointers.get(n), childrenPointers.getOrElse(n, Nil))))
      .toMap
  }

  def computeChecksum(nodes: Map[String, Node]): Int = {
    nodes.map { case (k, _) => countParents(k, nodes) }.sum
  }

  @scala.annotation.tailrec
  def countParents(start: String, nodes: Map[String, Node], count: Int = 0, stop: String = "COM"): Int = {
    if (start == stop) return count
    val cur = nodes(start).parent.get
    countParents(cur, nodes, count + 1, stop)
  }

  @scala.annotation.tailrec
  def parentList(start: String, nodes: Map[String, Node], ret: List[String] = Nil): List[String] = {
    if (start == "COM") return ret
    val cur = nodes(start).parent.get
    parentList(cur, nodes, start :: ret)
  }

  @scala.annotation.tailrec
  def computeCommonAncestor(l1: List[String], l2: List[String], last: Option[String] = Some("COM")): String = {
    if (l1.head != l2.head) return last.get
    else computeCommonAncestor(l1.tail, l2.tail, Some(l1.head))
  }

  def computeOrbitalDistance(nodes: Map[String, Node], from: String = "YOU", to: String = "SAN"): Int = {
    val fromList = parentList(from, nodes)
    val toList = parentList(to, nodes)
    val common = computeCommonAncestor(fromList, toList)
    val leg1 = countParents(from, nodes, 0, common)
    val leg2 = countParents(to, nodes, 0, common)
    leg1 + leg2 - 2
  }
}
