package days

import utility.*

import scala.annotation.tailrec

class Day23 extends IDay {
  type Trail = Grid2D[Cell]

  override def execute(input: String): (Int, Int) = {
    val trail: Trail = Grid2D.from2DCharArray(input, readCell)
    (part1(trail), part2(trail))
  }

  def part1(trail: Trail): Int = findLongestRoute(trail)

  def part2(oldTrail: Trail): Int = {
    val trail = Grid2D(oldTrail.entries.map(_.map({
      case Slope(_) => Clear()
      case cell => cell
    })))
    findLongestRoute(trail)
  }

  def readCell(c: Char): Cell = c match {
    case '#' => Blocked()
    case '.' => Clear()
    case '^' => Slope(Direction.North)
    case 'v' => Slope(Direction.South)
    case '<' => Slope(Direction.West)
    case '>' => Slope(Direction.East)
  }

  abstract class Cell {
    def next: List[Direction]

    def traversable(fromDir: Direction): Boolean

    def nextPositions(fromPos: Point2D): List[Point2D] = next.map(_.adjustPosition(fromPos))

    val blocked: Boolean = false
  }

  case class Blocked() extends Cell {
    def next: List[Direction] = Nil

    override def traversable(fromDir: Direction): Boolean = false

    override val blocked: Boolean = true
  }

  case class Clear() extends Cell {
    def next: List[Direction] = Direction.values.toList

    override def traversable(fromDir: Direction): Boolean = true
  }

  case class Slope(dir: Direction) extends Cell {
    def next: List[Direction] = List(dir)

    override def traversable(fromDir: Direction): Boolean = dir == fromDir
  }

  def findLongestRoute(trail: Trail) = {
    val junctions: Map[Point2D, (List[Direction], List[Direction])] = trail
      .entries
      .zipWithIndex
      .flatMap((row, y) => {
        row.zipWithIndex.map((cell: Cell, x) => {
          val pos = Point2D(x, y)
          val outgoingDirs = cell.next.filter(dir => {
            val nextPos = dir.adjustPosition(pos)
            trail.contains(nextPos) && trail.at(nextPos).traversable(dir)
          })
          val ingoingDirs = Direction.values.filter(dir => {
            val nextPos = dir.adjustPosition(pos)
            trail.contains(nextPos) && trail.at(nextPos).next.contains(dir.opposite)
          }).toList
          pos -> (ingoingDirs, outgoingDirs)
        })
      }).toMap
      .filter({ case (_, (ingoing, outgoing)) => (ingoing ++ outgoing).toSet.size > 2 })

    def findConnectionLength(start: Point2D, end: Point2D): Option[Int] = {
      val visited = collection.mutable.Set.empty[Point2D]
      val queue = collection.mutable.Queue.empty[(Point2D, Int)]
      queue.enqueue((start, 0))

      while (queue.nonEmpty) {
        val (current, length) = queue.dequeue()
        if (current == end) return Some(length)
        else if (!(junctions.contains(current) && current != start && current != end)) {
          val cell = trail.at(current)
          val next = cell.next.filter(dir => {
            val nextPos = dir.adjustPosition(current)
            trail.contains(nextPos) && trail.at(nextPos).traversable(dir) && !visited.contains(nextPos)
          }).map(_.adjustPosition(current))
          next.foreach(nextPos => {
            queue.enqueue((nextPos, length + 1))
            visited.add(nextPos)
          })
        }
      }
      None
    }

    val trailStart = (0 until trail.colCount).map(Point2D(_, 0)).find(pos => !trail.at(pos).blocked).get
    val trailEnd = (0 until trail.colCount).map(Point2D(_, trail.rowCount - 1)).find(pos => !trail.at(pos).blocked).get
    val connectionPoints = junctions.keys.toList :+ trailStart :+ trailEnd
    val connectionsPrelim: Map[(Point2D, Point2D), Int] = connectionPoints.combinations(2).flatMap({
      case List(a, b) => List((b, a) -> findConnectionLength(b, a), (a, b) -> findConnectionLength(a, b))
    }).filter(_._2.isDefined).map((pair, dist) => (pair, dist.get)).toMap

    val orderedPoints = connectionsPrelim.flatMap((k, v) => List(k._1, k._2)).toArray.distinct
    val trailStartIndex = orderedPoints.indexOf(trailStart)
    val trailEndIndex = orderedPoints.indexOf(trailEnd)
    val exitConnection = orderedPoints.indexOf(connectionsPrelim.keys.find({ case (from, to) => to == trailEnd }).get._1)

    val connections: Map[Int, Map[Int, Int]] = connectionsPrelim.keys.map(_._1).map(a => {
      orderedPoints.indexOf(a) -> connectionsPrelim
        .filter({ case ((a2, _), _) => a == a2 })
        .map({ case ((_, b), dist) => orderedPoints.indexOf(b) -> dist })
    }).toMap + (exitConnection -> Map(trailEndIndex -> connectionsPrelim((orderedPoints(exitConnection), trailEnd))))

    var best = 0

    def maxPathLength(start: Int, end: Int, distance: Int, visited: Array[Boolean]): Option[Int] = {
      if (start == end) {
        if (distance > best) best = distance
        Some(distance)
      } else {
        val maxPossible = connections.filterNot((a, _) => visited(a)).map(_._2.values.max).sum

        if (distance + maxPossible <= best) None else {
          connections(start)
            .filterNot((b, _) => visited(b))
            .map({ case (b, _) => maxPathLength(b, end, distance + connections(start)(b), visited.updated(b, true)) })
            .maxOption.flatten
        }
      }
    }

    maxPathLength(trailStartIndex, trailEndIndex, 0, Array.fill(orderedPoints.size)(false)).get
  }
}
