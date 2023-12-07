package days

import utility.*

class Day07 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val entries: Seq[(String, Int)] =
      Helper.mapAllMatches("(\\w{5}) (\\d+)".r, input, { m => (m.group(1), m.group(2).toInt) }).toSeq
    (part1(entries), part2(entries))
  }

  def part1(entries: Seq[(String, Int)]): Int = calculateWinnings(entries, false)

  def part2(entries: Seq[(String, Int)]): Int = calculateWinnings(entries, true)

  def calculateWinnings(entries: Seq[(String, Int)], jokerWildcard: Boolean): Int = {
    val comparator = new Ordering[(String, Int)] {
      override def compare(x: (String, Int), y: (String, Int)): Int = compareHands(jokerWildcard)(x._1, y._1)
    }

    val rankedEntries: Seq[(String, Int)] = entries
      .groupBy({ (hand, _) => kindStrength(hand, jokerWildcard) })
      .toSeq
      .sortBy({ case (kindStrength, _) => kindStrength })
      .flatMap({ case (_, group) => group.sorted(comparator) })

    rankedEntries.zipWithIndex.map({ case ((hand, bid), rank) => bid * (rank + 1) }).sum
  }

  def kindStrength(hand: String, jokerWildcard: Boolean): Int = {
    val counts = hand.filter(!jokerWildcard || _ != 'J').groupBy(identity).map(_._2.length)
    val jokerCount = if (jokerWildcard) hand.count(_ == 'J') else 0
    val groups = counts.toList.sorted.reverse

    val finalGroups =
      if (jokerCount == 5) List(5)
      else (groups.head + jokerCount) :: groups.tail

    strengthOfKinds(finalGroups)
  }

  def compareHands(jokerWildcard: Boolean)(hand1: String, hand2: String): Int = {
    val hand1Values = hand1.view.map(cardValue(jokerWildcard))
    val hand2Values = hand2.view.map(cardValue(jokerWildcard))

    hand1Values.lazyZip(hand2Values)
      .find { case (val1, val2) => val1 != val2 }
      .map { case (val1, val2) => val1.compareTo(val2) }
      .getOrElse(0)
  }

  def strengthOfKinds(counts: List[Int]): Int = counts match {
    case List(5) => 7
    case List(4, 1) => 6
    case List(3, 2) => 5
    case List(3, 1, 1) => 4
    case List(2, 2, 1) => 3
    case List(2, 1, 1, 1) => 2
    case List(1, 1, 1, 1, 1) => 1
    case _ => throw new Exception(s"Invalid kinds count list: $counts")
  }

  def cardValue(jokerWildcard: Boolean)(card: Char): Int = card match {
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => if (jokerWildcard) 1 else 11
    case 'T' => 10
    case n => n.toString.toInt
  }
}
