package days

import utility.*

import scala.util.matching.Regex.Match

class Day02 extends IDay {
  type Game = (Int, Seq[Seq[(Colour, Int)]])

  enum Colour {
    case Red, Green, Blue
  }

  object Colour {
    override def toString: String = super.toString.toLowerCase

    def fromString(s: String): Colour = s.toLowerCase match {
      case "red" => Red
      case "green" => Green
      case "blue" => Blue
    }
  }

  override def execute(input: String): (Int, Int) = {
    val gameStrings: Iterable[(Int, String)] =
      Helper.mapAllMatches("Game (\\d+): ([\\w ,;]+)".r, input, (m: Match) => (m.group(1).toInt, m.group(2)))
    val gameSplits: Iterable[(Int, Seq[String])] = gameStrings.map((id, s) => (id, s.split(";")))
    val games: Iterable[Game] = gameSplits.map({
      case (id: Int, rounds: Seq[String]) => (id, rounds.map(_.split(",").map(_.strip)
        .map(s => (Colour.fromString(s.split(" ")(1)), s.split(" ")(0).toInt))
      ))
    })
    (part1(games), part2(games))
  }

  def part1(games: Iterable[Game]): Int = {
    val limits: Map[Colour, Int] = Map(Colour.Red -> 12, Colour.Green -> 13, Colour.Blue -> 14)

    def isValidGame(game: Game): Boolean = getMaxCounts(game).forall((c, n) => n <= limits(c))

    games.filter(isValidGame).map(_._1).sum
  }

  def part2(games: Iterable[Game]): Int = {
    def getCubeCounts(game: Game): List[Int] = getMaxCounts(game).map(_._2).toList

    games.map(getCubeCounts).map(_.product).sum
  }

  def getMaxCounts(game: Game): Iterable[(Colour, Int)] = {
    def getMaxCountOfColour(game: Game, colour: Colour): Int =
      game._2.map(_.find((c2, _) => colour == c2)).map(_.getOrElse((colour, 0))._2).max

    Colour.values.map(c => (c, getMaxCountOfColour(game, c)))
  }
}
