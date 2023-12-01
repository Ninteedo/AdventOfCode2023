package days

import utility.{Helper, IDay}

class Day01 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val lines: Iterable[String] = Helper.readLines(input, identity)
    (part1(lines), part2(lines))
  }

  def part1: Iterable[String] => Int = getTotalValue(readDigit)

  def part2: Iterable[String] => Int = {
    def readAtIndex(s: String, i: Int): Option[Int] = readDigit(s, i) match {
      case None => readNumberWord(s, i)
      case some => some
    }

    getTotalValue(readAtIndex)
  }

  def getTotalValue(f: (String, Int) => Option[Int])(lines: Iterable[String]): Int =
    lines.map(l => getLineValue((0 until l.length).map(i => f(l, i)))).sum

  def getLineValue(digits: Seq[Option[Int]]): Int =
    digits.find(_.isDefined).get.get * 10 + digits.findLast(_.isDefined).get.get

  def readDigit(s: String, i: Int): Option[Int] = if (s(i).isDigit) Some(s(i).asDigit) else None

  val numberWordPatterns: Map[String, Int] = Map(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def readNumberWord(s: String, startIndex: Int): Option[Int] = {
    def isMatch(p: (String, Int)): Boolean =
      startIndex + p._1.length <= s.length && s.substring(startIndex, startIndex + p._1.length) == p._1

    numberWordPatterns.find(isMatch) match {
      case Some(value) => Some(value._2)
      case None => None
    }
  }
}
