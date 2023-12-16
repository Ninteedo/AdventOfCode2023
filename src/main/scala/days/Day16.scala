package days

import utility.*

class Day16 extends IDay {
  type Cave = Grid2D[Cell]

  override def execute(input: String): (Int, Int) = {
    val grid: Grid2D[Cell] = Grid2D.from2DCharArray(input, readCell)
    (part1(grid), part2(grid))
  }

  def part1(cave: Cave): Int = countEnergisedTiles(cave)(Point2D(0, 0), Direction.East)

  def part2(cave: Cave): Int = {
    val edgeIterator: Iterable[(Point2D, Direction)] = {
      val top = (0 until cave.colCount).map(col => (Point2D(col, 0), Direction.South))
      val right = (0 until cave.rowCount).map(row => (Point2D(cave.colCount - 1, row), Direction.West))
      val bottom = (0 until cave.colCount).map(col => (Point2D(col, cave.rowCount - 1), Direction.North))
      val left = (0 until cave.rowCount).map(row => (Point2D(0, row), Direction.East))
      top ++ right ++ bottom ++ left
    }

    edgeIterator.map((pos, dir) => countEnergisedTiles(cave)(pos, dir)).max
  }

  def countEnergisedTiles(cave: Cave)(startPos: Point2D, startDir: Direction): Int = {
    val frontier: collection.mutable.Queue[(Point2D, Direction)] = collection.mutable.Queue((startPos, startDir))
    val visited: collection.mutable.Set[Point2D] = collection.mutable.Set()
    val explored: collection.mutable.Set[(Point2D, Direction)] = collection.mutable.Set()

    while (frontier.nonEmpty) {
      val (current, direction) = frontier.dequeue()
      if (!explored.contains((current, direction))) {
        visited.add(current)
        explored.add((current, direction))
        val cell = cave.at(current)
        val childDirections = cell.childDirections(direction)
        val childPositions = childDirections.map(d => current + d.toPoint).zip(childDirections)
        frontier.enqueueAll(childPositions.filter((pos, dir) => cave.contains(pos) && !explored.contains((pos, dir))))
      }
    }

    visited.size
  }

  enum Direction {
    case North, East, South, West

    def toPoint: Point2D = this match {
      case North => Point2D(0, -1)
      case East => Point2D(1, 0)
      case South => Point2D(0, 1)
      case West => Point2D(-1, 0)
    }
  }

  def readCell(c: Char): Cell = c match {
    case '.' => Empty()
    case '/' => Mirror(true)
    case '\\' => Mirror(false)
    case '-' => Splitter(true)
    case '|' => Splitter(false)
    case _ => throw new Exception(s"Unknown cell type: $c")
  }

  abstract class Cell {
    def childDirections(incident: Direction): List[Direction]
  }

  case class Empty() extends Cell {
    def childDirections(incident: Direction): List[Direction] = List(incident)
  }

  case class Mirror(bottomLeftToTopRight: Boolean) extends Cell {
    override def childDirections(incident: Direction): List[Direction] = bottomLeftToTopRight match {
      case true => incident match {
        case Direction.North => List(Direction.East)
        case Direction.East => List(Direction.North)
        case Direction.South => List(Direction.West)
        case Direction.West => List(Direction.South)
      }
      case false => incident match {
        case Direction.North => List(Direction.West)
        case Direction.East => List(Direction.South)
        case Direction.South => List(Direction.East)
        case Direction.West => List(Direction.North)
      }
    }
  }

  case class Splitter(leftToRight: Boolean) extends Cell {
    override def childDirections(incident: Direction): List[Direction] = leftToRight match {
      case true => incident match {
        case Direction.North => List(Direction.West, Direction.East)
        case Direction.East => List(Direction.East)
        case Direction.South => List(Direction.East, Direction.West)
        case Direction.West => List(Direction.West)
      }
      case false => incident match {
        case Direction.North => List(Direction.North)
        case Direction.East => List(Direction.South, Direction.North)
        case Direction.South => List(Direction.South)
        case Direction.West => List(Direction.North, Direction.South)
      }
    }
  }
}
