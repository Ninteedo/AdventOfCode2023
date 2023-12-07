package days

import utility.*

class Day07 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val entries: Seq[(String, Int)] = Helper.mapAllMatches("(\\w{5}) (\\d+)".r, input, {m => (m.group(1), m.group(2).toInt)}).toSeq
    (part1(entries), part2(entries))
  }

  def calculateWinnings(entries: Seq[(String, Int)], compareHands: (String, String) => Int): Int = {
    val comparator = new Ordering[String]{
      override def compare(x: String, y: String): Int = compareHands(x, y)
    }

    val ranked: Seq[(String, Int)] = entries.sortBy({ h => h._1 })(comparator)
    ranked.zipWithIndex.map((entry, rank) => entry._2 * (rank + 1)).sum
  }

  def compareHands(jokerWildcard: Boolean)(hand1: String, hand2: String): Int = {
    def kindStrength(hand: String): Int = {
      val sorted = hand.sorted
      val groups: List[Int] = if (!jokerWildcard) {
        sorted.toSet.groupMap(identity)({ c => hand.count(_ == c) })
          .map({ (k, v) => v.toList.head }).toList.sorted.reverse
      } else {
        val intermediate = sorted.filter(_ != 'J').toSet.groupMap(identity)({ c => hand.count(_ == c) })
          .map({ (k, v) => v.toList.head }).toList.sorted.reverse
        if (intermediate.isEmpty) List(5) else {
          intermediate.zip(LazyList(hand.count(_ == 'J')) ++ LazyList.continually(0)).map((a, b) => a + b)
        }
      }
      groups match {
        case List(5) => 7
        case List(4, 1) => 6
        case List(3, 2) => 5
        case List(3, 1, 1) => 4
        case List(2, 2, 1) => 3
        case List(2, 1, 1, 1) => 2
        case List(1, 1, 1, 1, 1) => 1
        case _ => throw new Exception(s"Invalid kinds count list: $sorted, $groups")
      }
    }

    def cardValue(card: Char): Int = card match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => if (jokerWildcard) 1 else 11
      case 'T' => 10
      case n => n.toString.toInt
    }

    val (kind1, kind2) = (kindStrength(hand1), kindStrength(hand2))
    if (kind1 == kind2) {
      val differingPair: (Int, Int) = hand1.map(cardValue).lazyZip(hand2.map(cardValue)).find((l, r) => l != r).get
      if (differingPair._1 < differingPair._2) -1 else 1
    } else if (kind1 < kind2) -1 else 1
  }

  def part1(entries: Seq[(String, Int)]) = calculateWinnings(entries, compareHands(false))

  def part2(entries: Seq[(String, Int)]) = calculateWinnings(entries, compareHands(true))
}
