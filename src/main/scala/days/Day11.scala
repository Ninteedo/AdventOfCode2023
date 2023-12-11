package days

import utility.*

class Day11 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val galaxiesGrid: Grid2D[Boolean] = Grid2D.from2DCharArray(input, c => c == '#')
    (part1(galaxiesGrid), part2(galaxiesGrid))
  }

  def part1(grid: Grid2D[Boolean]): Long = totalGalaxyDistances(grid, 2)

  def part2(grid: Grid2D[Boolean]): Long = totalGalaxyDistances(grid, 1000000)

  def totalGalaxyDistances(galaxyGrid: Grid2D[Boolean], expansionDist: Int): Long = {
    def getExpandedIndices(entries: Array[Array[Boolean]]): Set[Int] = {
      entries.zipWithIndex.collect {
        case (entry, index) if !entry.contains(true) => index
      }.toSet
    }

    val expandedColIndices = getExpandedIndices(galaxyGrid.entriesByColumn)
    val expandedRowIndices = getExpandedIndices(galaxyGrid.entries)

    def expandPosition(pos: Point2D): Point2D = {
      val expandX = expandedColIndices.count(_ < pos.x) * (expansionDist - 1)
      val expandY = expandedRowIndices.count(_ < pos.y) * (expansionDist - 1)
      Point2D(pos.x + expandX, pos.y + expandY)
    }

    val galaxies = galaxyGrid.indicesWhere(_ == true).map(expandPosition)

    (for {
      i <- galaxies.indices
      j <- i + 1 until galaxies.length
      galaxy1 = galaxies(i)
      galaxy2 = galaxies(j)
    } yield (galaxy1.x.toLong - galaxy2.x).abs + (galaxy1.y.toLong - galaxy2.y).abs).sum
  }
}
