package days

import utility.*
import utility.Helper.lcm

class Day08 extends IDay {
  override def execute(input: String): (Int, Long) = {
    val lines = input.split("\n")
    val instructions: Array[Char] = lines.head.toCharArray
    val connections: Map[String, (String, String)] =
      Helper
        .mapAllMatches("(\\w{3}) = \\((\\w{3}), (\\w{3})\\)".r, input, { m => (m.group(1), (m.group(2), m.group(3))) })
        .toMap
    (part1(instructions, connections), part2(instructions, connections))
  }

  def part1(instructions: Array[Char], connections: Map[String, (String, String)]): Int = {
    var position = "AAA"
    var steps = 0
    while (position != "ZZZ") {
      position = findNextPosition(connections)(position, instructions(steps % instructions.length))
      steps += 1
    }
    steps
  }

  def part2(instructions: Array[Char], connections: Map[String, (String, String)]): Long = {
    val startingNodes = connections.keys.filter(_.last == 'A')

    def isEndNode(position: String): Boolean = position.last == 'Z'

    var endHistory: Map[Int, Int] = Map()

    var positions = startingNodes.zipWithIndex
    var step = 0
    while (positions.nonEmpty) {
      positions = positions.map((pos, i) => (findNextPosition(connections)(pos, instructions(step % instructions.length)), i))
      step += 1
      var toRemove: List[Int] = Nil
      positions.filter(p => isEndNode(p._1)).foreach({ (position, index) =>
        endHistory += (index -> step)
        toRemove = index :: toRemove
      })
      positions = positions.filter((_, index) => !toRemove.contains(index))
    }

    val firstEndHits: Iterable[Long] = endHistory.values.map(_.toLong)
    firstEndHits.fold(1L)(lcm)
  }

  def findNextPosition(connections: Map[String, (String, String)])
    (position: String, instruction: Char): String = instruction match {
    case 'L' => connections(position)._1
    case 'R' => connections(position)._2
  }
}
