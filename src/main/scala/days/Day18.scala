package days

import utility.*
import utility.Helper.boolToInt

import scala.util.matching.Regex.Match

class Day18 extends IDay {

  type Instruction = (Direction, Int, String)

  override def execute(input: String): (Long, Long) = {
    def readInstruction(m: Match): Instruction = {
      val direction = m.group(1)
      val distance = m.group(2)
      val colour = m.group(3)
      (readDirection(direction), distance.toInt, colour)
    }
    val instructions = Helper.mapAllMatches("(\\w) (\\d+) \\(#(\\w+)\\)".r, input, readInstruction)

    (part1(instructions), part2(instructions))
  }

  def part1(instructions: Iterable[Instruction]): Long = {
    def readInstruction(instruction: Instruction): Point2D = instruction match {
      case (direction, distance, _) => direction.toPoint * distance
    }

    calculateArea(calculateVertices(instructions, readInstruction))
  }

  def part2(instructions: Iterable[Instruction]): Long = {
    def readInstruction(instruction: Instruction): Point2D = instruction match {
      case (_, _, hex) => {
        val distance = readHex(hex.dropRight(1))
        val direction = List(Direction.East, Direction.South, Direction.West, Direction.North)(hex.takeRight(1).toInt)
        direction.toPoint * distance
      }
    }

    calculateArea(calculateVertices(instructions, readInstruction))
  }

  def calculateVertices(instructions: Iterable[Instruction], readInstruction: Instruction => Point2D): Array[Point2D] = {
    val vertices: collection.mutable.ArrayBuffer[Point2D] = collection.mutable.ArrayBuffer()
    instructions.foldLeft(Point2D.zero)({
      case (current, instr) =>
        vertices.addOne(current)
        current + readInstruction(instr)
    })

    vertices.toArray
  }

  def calculateArea(vertices: Array[Point2D]): Long = {
    val edges = (vertices :+ vertices.head).sliding(2).toList

    val sortedAlongX = edges
      .filter(edge => edge.head.x == edge.last.x)
      .sortBy(_.head.x)

    def occupiedAtY(y: Int): Long = {
      val ordered = sortedAlongX.filter(edge => {
        val (vertexA, vertexB) = (edge.head, edge.last)
        val (minY, maxY) = (vertexA.y.min(vertexB.y), vertexA.y.max(vertexB.y))
        y >= minY && y <= maxY && vertexA.x == vertexB.x
      })

      def crossingsMade(x: Int): Int = ordered.filter(_.head.x <= x).flatten.count(_.y > y)

      ordered.sliding(2).map(group => {
        val (edgeA, edgeB) = (group.head, group.last)
        val (vertexA, vertexB) = (edgeA.head, edgeA.last)
        val (vertexC, vertexD) = (edgeB.head, edgeB.last)

        def defaultCalc: Int = boolToInt(crossingsMade(vertexA.x.min(vertexC.x)) % 2 == 1) * (vertexC.x - vertexB.x + 1)

        if (!List(vertexC.y, vertexD.y).exists(y2 => List(vertexA.y, vertexB.y).contains(y2))) {
          defaultCalc
        } else {
          val connection = edges.find(edge => {
            val (vertexE, vertexF) = (edge.head, edge.last)
            vertexE.y == y && vertexF.y == y && {
              val eFind = List(List(vertexA, vertexB), List(vertexC, vertexD)).find(_.contains(vertexE))
              val fFind = List(List(vertexA, vertexB), List(vertexC, vertexD)).find(_.contains(vertexF))
              eFind.isDefined && fFind.isDefined && eFind != fFind
            }
          })
          if (connection.isDefined) {
            val (vertexE, vertexF) = (connection.get.head, connection.get.last)
            val diff = (vertexE.x - vertexF.x).abs + 1
            val (smallX, bigX) = (vertexE.x.min(vertexF.x), vertexE.x.max(vertexF.x))
            val loss = boolToInt(crossingsMade(smallX - 1) % 2 == 1) + boolToInt(crossingsMade(bigX) % 2 == 1)
            diff - loss
          } else {
            defaultCalc
          }
        }
      }).sum.toLong
    }

    sortedAlongX
      .sortBy(_.head.y)
      .flatten
      .map(_.y)
      .distinct
      .sorted
      .flatMap(y => List(y, y + 1))
      .sliding(2)
      .map(group => occupiedAtY(group.head) * (group.last - group.head))
      .sum
  }

  def readHex(s: String): Int = Integer.parseInt(s, 16)

  def readDirection(s: String): Direction = s match {
    case "U" => Direction.North
    case "D" => Direction.South
    case "L" => Direction.West
    case "R" => Direction.East
  }
}
