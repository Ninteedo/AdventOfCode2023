package days

import utility.*

import scala.annotation.tailrec

class Day06 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val lines = Helper.readLines(input, identity)
    val times: Iterable[Long] = lines.head.stripPrefix("Time:").split(" ").filter(_.nonEmpty).map(_.toLong)
    val distances: Iterable[Long] = lines.last.stripPrefix("Distance:").split(" ").filter(_.nonEmpty).map(_.toLong)
    val races: Iterable[(Long, Long)] = times.zip(distances)
    val bigRace: (Long, Long) = (times.mkString("").toLong, distances.mkString("").toLong)
    (part1(races), part2(bigRace))
  }

  def calculateWinningStrategiesCount(race: (Long, Long)): Long = {
    val (time, distance) = race

    def isWinning(holdTime: Long): Boolean = holdTime * (time - holdTime) > distance

    @tailrec
    def binarySearch(l: Long, r: Long, swapDir: Int): Long = (l + r) / 2 match {
      case m if l >= r => if isWinning(l) then l else l + 1 * swapDir
      case m if isWinning(m) == (swapDir == 1) => binarySearch(l, m - 1, swapDir)
      case m => binarySearch(m + 1, r, swapDir)
    }
    binarySearch(0L, time, -1) - binarySearch(0L, time, 1) + 1
  }

  def part1(races: Iterable[(Long, Long)]): Long = races.map(calculateWinningStrategiesCount).product

  def part2(race: (Long, Long)): Long = calculateWinningStrategiesCount(race)
}
