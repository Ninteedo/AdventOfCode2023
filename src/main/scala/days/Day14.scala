package days

import utility.*

class Day14 extends IDay {
  type RockGrid = Grid2D[Option[Boolean]]

  override def execute(input: String): (Int, Int) = {
    val grid: RockGrid = Grid2D.from2DCharArray(input, {
      case 'O' => Some(true)
      case '#' => Some(false)
      case '.' => None
      case c => throw new Exception(s"Invalid input character '$c'")
    })

    (part1(grid), part2(grid))
  }

  def moveRock(grid: RockGrid, rockPos: Point2D, direction: Point2D): RockGrid = {
    if (!grid.at(rockPos).contains(true)) return grid

    var currPos = rockPos
    var prevPos: Option[Point2D] = None

    while (grid.contains(currPos) && (prevPos.isEmpty || grid.at(currPos).isEmpty)) {
      prevPos = Some(currPos)
      currPos = currPos + direction
    }

    grid.updated(rockPos, None).updated(prevPos.get, Some(true))
  }

  def part1(grid: RockGrid): Int = evaluateRocks(tiltInDirection(grid, Point2D(0, -1)))

  def part2(grid: RockGrid): Int = {
    val spinCycleOrder = List(
      Point2D(0, -1),
      Point2D(-1, 0),
      Point2D(0, 1),
      Point2D(1, 0)
    )

    val maxSpins = 1000000000L
    var curr: RockGrid = grid
    val fullCycleEntries: collection.mutable.ArrayBuffer[RockGrid] = collection.mutable.ArrayBuffer[RockGrid]()

    while (!fullCycleEntries.contains(curr)) {
      fullCycleEntries += curr
      curr = spinCycleOrder.foldLeft(curr)((g, dir) => tiltInDirection(g, dir))
    }

    val cycleStart = fullCycleEntries.indexOf(curr)
    val cycleLength = fullCycleEntries.length - cycleStart

    val finalIndex = (maxSpins - cycleStart) % cycleLength + cycleStart
    evaluateRocks(fullCycleEntries(finalIndex.toInt))
  }

  def evaluateRocks(grid: RockGrid): Int = {
    grid.entries.zipWithIndex.flatMap((row, y) =>
      row.map(entry => if (entry.contains(true)) grid.rowCount - y else 0)
    ).sum
  }

  def tiltInDirection(grid: RockGrid, dir: Point2D): RockGrid = {
    val (width, height) = (grid.colCount, grid.rowCount)
    val iterator: Seq[Point2D] = dir match {
      case Point2D(0, -1) => for (y <- 0 until height; x <- 0 until width) yield Point2D(x, y)
      case Point2D(0, 1) => for (y <- (0 until height).reverse; x <- 0 until width) yield Point2D(x, y)
      case Point2D(-1, 0) => for (x <- 0 until width; y <- 0 until height) yield Point2D(x, y)
      case Point2D(1, 0) => for (x <- (0 until width).reverse; y <- 0 until height) yield Point2D(x, y)
      case _ => throw new Exception(s"Invalid direction $dir")
    }

    var curr: RockGrid = grid
    iterator.foreach({pos => curr = moveRock(curr, pos, dir)})
    curr
  }
}
