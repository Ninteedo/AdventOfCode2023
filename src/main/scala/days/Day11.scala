package days

import utility.*

class Day11 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val galaxiesGrid: Grid2D[Boolean] = Grid2D.from2DCharArray(input, c => c == '#')
    (part1(galaxiesGrid), part2(galaxiesGrid))
  }

  def part1(grid: Grid2D[Boolean]): Long = totalGalaxyDistances(grid, 2L)

  def part2(grid: Grid2D[Boolean]): Long = totalGalaxyDistances(grid, 1000000L)

  def totalGalaxyDistances(galaxyGrid: Grid2D[Boolean], expansionDist: Long): Long = {
    val expandedCols = galaxyGrid.entriesByColumn.zipWithIndex.filter((column, i) => !column.contains(true)).map(_._2).toSet
    val expandedRows = galaxyGrid.entries.zipWithIndex.filter((row, i) => !row.contains(true)).map(_._2).toSet

    val galaxies: List[Point2D] = galaxyGrid.indicesWhere(_ == true)
    galaxies.indices.flatMap(i =>
      (i + 1 until galaxies.length).map(j => {
        val galaxy1 = galaxies(i)
        val galaxy2 = galaxies(j)

        val coveredCols = (math.min(galaxy1.x, galaxy2.x) until math.max(galaxy1.x, galaxy2.x)).map(x =>
          if (expandedCols.contains(x)) expansionDist
          else 1
        ).sum
        val coveredRows = (math.min(galaxy1.y, galaxy2.y) until math.max(galaxy1.y, galaxy2.y)).map(y =>
          if (expandedRows.contains(y)) expansionDist
          else 1
        ).sum
        coveredCols + coveredRows
      })
    ).sum
  }
}
