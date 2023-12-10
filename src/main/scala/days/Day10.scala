package days

import utility.*

import scala.annotation.tailrec

class Day10 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val pipeGrid: Grid2D[Pipe] = Grid2D.from2DCharArray(input, Pipe.read)
    (part1(pipeGrid), part2(pipeGrid))
  }

  enum Direction {
    case North, East, South, West

    def adjustPosition(pos: Point2D): Point2D = this match {
      case North => Point2D(pos.x, pos.y - 1)
      case East => Point2D(pos.x + 1, pos.y)
      case South => Point2D(pos.x, pos.y + 1)
      case West => Point2D(pos.x - 1, pos.y)
    }

    def opposite: Direction = this match {
      case North => South
      case East => West
      case South => North
      case West => East
    }
  }

  abstract class Pipe {
    def connections: Set[Direction]

    def connects(dir: Direction): Boolean
  }

  case class TwoPipe(conn1: Direction, conn2: Direction) extends Pipe {
    override def connections: Set[Direction] = Set(conn1, conn2)

    def connects(dir: Direction): Boolean = conn1 == dir || conn2 == dir
  }

  case class GroundPipe() extends Pipe {
    override def connections: Set[Direction] = Set.empty

    def connects(dir: Direction): Boolean = false
  }

  case class StartPipe() extends Pipe {
    private var connectionStore: Set[Direction] = Set()

    override def connections: Set[Direction] = connectionStore

    def connects(dir: Direction): Boolean = connections.contains(dir)

    def setDirections(dirs: Set[Direction]): Unit = connectionStore = dirs
  }

  object Pipe {
    def read(c: Char): Pipe = c match {
      case '|' => TwoPipe(Direction.North, Direction.South)
      case '-' => TwoPipe(Direction.East, Direction.West)
      case 'L' => TwoPipe(Direction.North, Direction.East)
      case 'J' => TwoPipe(Direction.North, Direction.West)
      case '7' => TwoPipe(Direction.South, Direction.West)
      case 'F' => TwoPipe(Direction.South, Direction.East)
      case '.' => GroundPipe()
      case 'S' => StartPipe()
    }
  }

  def getMainPipe(pipeGrid: Grid2D[Pipe]): Set[Point2D] = {
    var mainLoop: Set[Point2D] = Set()

    val startPos: Point2D = pipeGrid.indexWhere(_ == StartPipe()).get
    val startConnections: Set[Direction] = Direction.values.filter(dir => {
      val newPos = dir.adjustPosition(startPos)
      pipeGrid.contains(newPos) && pipeGrid.at(newPos).connects(dir.opposite)
    }).toSet
    pipeGrid.at(startPos).asInstanceOf[StartPipe].setDirections(startConnections)

    @tailrec
    def traversePipes(curr: Point2D, prevDir: Direction): Unit = {
      mainLoop += curr
      pipeGrid.at(curr) match {
        case StartPipe() =>
        case prevPipe => prevPipe.connections.filter(_ != prevDir.opposite).head match {
          case dir => traversePipes(dir.adjustPosition(curr), dir)
        }
      }
    }

    traversePipes(startConnections.head.adjustPosition(startPos), startConnections.head)
    mainLoop
  }

  def part1(pipeGrid: Grid2D[Pipe]): Int = getMainPipe(pipeGrid).size / 2

  def part2(pipeGrid: Grid2D[Pipe]): Int = {
    val mainLoop: Set[Point2D] = getMainPipe(pipeGrid)

    // (isOpen, isMajor)
    def superGridPoint(pos: Point2D): Boolean = {
      val oldPos = Point2D(pos.x / 2, pos.y / 2)

      def getPipe(dir: Option[Direction]): Pipe = {
        if (mainLoop.contains(oldPos)) {
          pipeGrid.at(dir match {
            case Some(d) => d.adjustPosition(oldPos)
            case None => oldPos
          }
          )
        } else {
          GroundPipe()
        }
      }

      val majorPipe = getPipe(None)

      (pos.x % 2, pos.y % 2) match {
        case (0, 0) => majorPipe match {
          case GroundPipe() => false
          case pipe => true
        }
        case (1, 0) => {
          val westernPipe = majorPipe
          val easternPipe = getPipe(Some(Direction.East))
          westernPipe.connects(Direction.East) && easternPipe.connects(Direction.West)
        }
        case (0, 1) => {
          val northernPipe = majorPipe
          val southernPipe = getPipe(Some(Direction.South))
          northernPipe.connects(Direction.South) && southernPipe.connects(Direction.North)
        }
        case (1, 1) => false
      }
    }

    val superGrid: Grid2D[Boolean] = Grid2D((0 to (pipeGrid.rowCount - 1) * 2).map(y =>
      (0 to (pipeGrid.colCount - 1) * 2).map(x => superGridPoint(Point2D(x, y))).toArray
    ).toArray)

    var connectsToEdge: collection.mutable.Map[Point2D, Boolean] = collection.mutable.Map()

    def connectsToEdgeRegionCheck(pos: Point2D): Boolean = {
      val currRegion: collection.mutable.Set[Point2D] = collection.mutable.Set()
      val frontier: collection.mutable.Queue[Point2D] = collection.mutable.Queue()
      frontier.enqueue(pos)

      def finalise(pos: Point2D, result: Boolean): Boolean = {
        currRegion.addAll(frontier).foreach(pos => connectsToEdge.put(pos, result))
        result
      }

      while (frontier.nonEmpty) {
        val currPos = frontier.dequeue()
        connectsToEdge.get(currPos) match {
          case Some(value) => Some(value)
          case None => Direction.values.exists(dir => !superGrid.contains(dir.adjustPosition(currPos))) match {
            case true => Some(true)
            case false => None
          }
        } match {
          case Some(result) => return finalise(currPos, result)
          case _ =>
        }

        Direction.values.foreach(dir => {
          val newPos = dir.adjustPosition(currPos)
          if (!currRegion.contains(newPos) && !superGrid.at(newPos)) {
            if (connectsToEdge.contains(newPos)) return finalise(currPos, connectsToEdge(newPos))
            frontier.enqueue(newPos)
            currRegion.add(newPos)
          }
        })
      }
      finalise(pos, false)
    }

    def checkEnclosed(pos: Point2D): Boolean = connectsToEdge.get(pos) match {
      case Some(value) => !value
      case None => !superGrid.at(pos) && !connectsToEdgeRegionCheck(pos)
    }

    (0 until pipeGrid.rowCount).flatMap(y => {
      (0 until pipeGrid.colCount).map(x => {
        Point2D(x * 2, y * 2)
      })
    }).count(checkEnclosed)
  }
}
