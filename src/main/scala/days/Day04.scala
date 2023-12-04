package days

import utility.*

import scala.util.matching.Regex

class Day04 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val pattern = "Card\\s+(\\d+):\\s+([\\d ]+)\\s+\\|\\s+([\\d ]+)\\s+".r

    def readCard(m: Regex.Match): Card = {
      val index = m.group(1).toInt
      val winningNumbers = m.group(2).split(" ").flatMap(_.strip.toIntOption)
      val actualNumbers = m.group(3).split(" ").flatMap(_.strip.toIntOption)
      Card(index, winningNumbers, actualNumbers)
    }

    val cards: Iterable[Card] = Helper.mapAllMatches(pattern, input, readCard)

    (part1(cards), part2(cards))
  }

  def part1(cards: Iterable[Card]): Int = cards.map(_.cardScore).sum

  def part2(cards: Iterable[Card]): Int = {
    var prizeMap: Map[Int, Int] = Map()

    def determineAllCardPrizes(id: Int): Int = if (prizeMap.contains(id)) prizeMap(id) else {
      val card: Card = cards.find(_.id == id).get
      val total: Int = card.prizeCards.map(determineAllCardPrizes).sum + 1
      prizeMap += (id -> total)
      total
    }

    cards.map(card => determineAllCardPrizes(card.id)).sum
  }

  class Card(val id: Int, val winning: Seq[Int], val actual: Seq[Int]) {
    lazy val matchingNumberCount: Int = actual.count(actualNum => winning.contains(actualNum))

    lazy val cardScore: Int = math.pow(2, matchingNumberCount - 1).toInt

    lazy val prizeCards: List[Int] = (id + 1 to id + matchingNumberCount).toList
  }
}
