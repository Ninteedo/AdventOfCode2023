package days

import utility.*

class Day21 extends IDay {
  type Garden = Grid2D[Spot]

  override def execute(input: String): (Int, Long) = {
    val garden: Garden = Grid2D.from2DCharArray(input, Spot.read)
    (part1(garden), part2(garden))
  }

  def part1(garden: Garden): Int = runSearch(garden)(64)

  def part2(garden: Garden): Long = {
    val width: Int = garden.colCount
    val ys = (0 until 3).map(_ * width + width / 2).map(runSearch(garden, true))
    val (a, b, c) = fitQuadratic(ys)

    val steps = 26501365
    val bigStep = steps / width
    (a * bigStep * bigStep) + (b * bigStep) + c
  }

  enum Spot {
    case Open, Rock, Start
  }

  object Spot {
    def read(c: Char): Spot = c match {
      case '.' => Open
      case '#' => Rock
      case 'S' => Start
    }
  }

  def runSearch(garden: Garden, allowSpill: Boolean = false)(steps: Int): Int = {
    val start = garden.indexWhere(_ == Spot.Start).get
    val frontier: collection.mutable.Queue[Point2D] = collection.mutable.Queue(start)
    val visited: collection.mutable.Set[Point2D] = collection.mutable.Set(start)

    for (i <- 0 until steps) {
      val nextFrontier: collection.mutable.ArrayBuffer[Point2D] = collection.mutable.ArrayBuffer()
      while (frontier.nonEmpty) {
        val current: Point2D = frontier.dequeue()
        for (neighbour <- Direction.values.map(_.adjustPosition(current))) {
          val wrapped = Point2D(math.floorMod(neighbour.x, garden.colCount), math.floorMod(neighbour.y, garden.rowCount))
          if (garden.at(wrapped) != Spot.Rock && !visited.contains(neighbour)) {
            nextFrontier.append(neighbour)
            visited.add(neighbour)
          }
        }
      }
      frontier.addAll(nextFrontier)
    }

    visited.count(_.mannDist(start) % 2 == steps % 2)
  }

  def fitQuadratic(ys: Seq[Int]): (Long, Long, Long) = {
    if (ys.length < 3) throw new IllegalArgumentException("Must have 3 points")

    val (y0, y1, y2) = (ys(0), ys(1), ys(2))

    val a = (y0 - 2L * y1 + y2) / 2L
    val b = y1 - y0 - a
    (a, b, y0)
  }
}
