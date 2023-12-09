package days

import utility.*

import scala.annotation.tailrec

class Day09 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val rows: Seq[Seq[Long]] = Helper.readLines(input, {line => line.strip.split(" ").map(_.toLong).toSeq}).toSeq
    (part1(rows), part2(rows))
  }

  def part1(rows: Seq[Seq[Long]]): Long = {
    def predictFirstValue(row: Seq[Long]): Long = getDifferences(row).map(_.last).sum

    rows.map(predictFirstValue).sum
  }

  def part2(rows: Seq[Seq[Long]]): Long = {
    def predictNextValue(row: Seq[Long]): Long =
      getDifferences(row).map(_.head).foldRight(0L)({(curr, total) => curr - total})

    rows.map(predictNextValue).sum
  }

  def getDifferences(row: Seq[Long]): Seq[Seq[Long]] = {
    @tailrec
    def nextDifferencesRec(differences: Seq[Seq[Long]]): Seq[Seq[Long]] = if (differences.last.forall(_ == 0L)) {
      differences
    } else {
      val next: Seq[Long] = differences.last.sliding(2).map({ case Seq(a, b) => b - a }).toSeq
      nextDifferencesRec(differences :+ next)
    }

    nextDifferencesRec(Seq(row))
  }
}
