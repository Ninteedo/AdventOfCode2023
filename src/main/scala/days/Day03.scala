package days

import utility.*

import scala.collection.immutable.Map
import scala.reflect.ClassTag

class Day03 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val grid: Grid2D[Char] = Grid2D(Helper.readLines(input, _.toCharArray).toArray)
    (part1(grid), part2(grid))
  }

  def part1(grid: Grid2D[Char]): Int = {
    def isSymbol(c: Char): Boolean = !c.isDigit && c != '.'

    val symbolPositions: Array[Point2D] = getPositionsWhere(
      grid, (char, pos) => if (isSymbol(char)) Some(pos) else None
    )
    val adjacentPositions: Set[Point2D] = symbolPositions.flatMap(pos => Point2D.adjacents.map(_ + pos)).toSet

    val numberGroups: Map[Int, Array[(Char, Point2D)]] = getGroupedNumbers(grid)
    val filteredNumberGroups: Map[Int, Array[(Char, Point2D)]] = numberGroups.filter((_, arr) =>
      arr.exists((_, pos) => adjacentPositions.contains(pos))
    )

    filteredNumberGroups.map((_, arr) => readNumberGroupValue(arr)).sum
  }

  def part2(grid: Grid2D[Char]): Int = {
    def isGearSymbol(c: Char): Boolean = c == '*'

    val gearPositions: Array[Point2D] = getPositionsWhere(
      grid, (char, pos) => if (isGearSymbol(char)) Some(pos) else None
    )

    val numberGroups: Map[Int, Array[(Char, Point2D)]] = getGroupedNumbers(grid)
    val gearGroups: Iterable[(Int, Iterable[Int])] = gearPositions.zipWithIndex.map((gearPos, i) => {
      (i, numberGroups.values.filter(_.exists((_, pos) => grid.adjacent(gearPos, pos)))
        .map(readNumberGroupValue))
    })

    gearGroups.filter(_._2.size == 2).map(_._2.product).sum
  }

  def getPositionsWhere[A: ClassTag](grid: Grid2D[Char], f: (Char, Point2D) => Option[A]): Array[A] = {
    grid.entries.zipWithIndex.flatMap((row: Array[Char], r: Int) =>
      row.zipWithIndex.map((char: Char, c: Int) => f(char, Point2D(c, r)))
    ).collect({ case Some(x: A) => x })
  }

  def getGroupedNumbers(grid: Grid2D[Char]): Map[Int, Array[(Char, Point2D)]] = {
    val numberPositions: Array[(Char, Point2D)] = getPositionsWhere(
      grid,
      (char, pos) => if (char.isDigit) Some((char, pos)) else None
    )
    var groupMap: Map[Point2D, Int] = Map()
    var groupIndex: Int = 0

    def checkNumberGroup(pos: Point2D): Int = {
      val prev = Point2D(pos.x - 1, pos.y)
      val mapValue: Int = groupMap.getOrElse(
        prev, {
          groupIndex += 1
          groupIndex
        }
      )
      groupMap += (pos -> groupIndex)
      mapValue
    }

    numberPositions.groupBy((_, pos) => checkNumberGroup(pos))
  }

  def readNumberGroupValue(group: Array[(Char, Point2D)]): Int = group.map((char, _) => char).mkString.toInt
}
