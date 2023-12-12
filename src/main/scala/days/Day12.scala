package days

import utility.*

class Day12 extends IDay {
  type Entry = (List[Option[Boolean]], List[Int])

  override def execute(input: String): (Any, Any) = {
    def readLine(line: String): Entry = {
      def readChar(c: Char) = c match {
        case '.' => Some(false)
        case '#' => Some(true)
        case '?' => None
      }

      val splits = line.split(" ")
      (splits(0).map(readChar).toList, splits(1).split(",").map(_.toInt).toList)
    }

    val entries = Helper.readLines(input, readLine)
    (part1(entries), part2(entries))
  }

  def calculatePossiblePermutations(entry: Entry): Long = {
    val (initial, groups) = entry

    def isValidHead(groupified: List[Int]): Boolean = {
      groupified.isEmpty ||
        (groupified.length <= groups.length && {
          val zipped = groupified.zip(groups)
          zipped.init.forall((a, b) => a == b) && (zipped.last._1 <= zipped.last._2)
        })
    }

    def groupify(existing: List[Boolean]): List[Int] = {
      val result: collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer[Int]()
      var tail = existing
      var currGroup: Int = 0

      while (tail.nonEmpty) {
        if tail.head then {
          currGroup += 1
        } else {
          if currGroup > 0 then
            result += currGroup
            currGroup = 0
        }
        tail = tail.tail
      }

      if currGroup > 0 then result += currGroup

      result.toList
    }

    val cache: collection.mutable.Map[(Int, List[Int]), Long] = collection.mutable.Map()

    def calculatePermutations(
      existing: List[Boolean],
      remaining: List[Option[Boolean]],
    ): Long = {
      val groupified = groupify(existing)

      if !isValidHead(groupified) then return 0L

      if existing.nonEmpty && !existing.last && cache.contains((existing.length, groupified)) then
        return cache((existing.length, groupified))

      if remaining.isEmpty then {
        if groupified == groups then 1L else 0L
      } else {
        remaining.head match {
          case Some(value) => calculatePermutations(existing :+ value, remaining.tail)
          case None => {
            val l = calculatePermutations(existing :+ true, remaining.tail)
            val r = {
              val res = calculatePermutations(existing :+ false, remaining.tail)
              cache.put((existing.length + 1, groupified), res)
              res
            }
            l + r
          }
        }
      }
    }

    calculatePermutations(Nil, initial)
  }

  def totalPermutations(entries: Iterable[Entry]): Long = entries.map(calculatePossiblePermutations).sum

  def part1: Iterable[Entry] => Long = totalPermutations

  def part2(entries: Iterable[Entry]): Long = {
    def unfoldEntry(entry: Entry): Entry = {
      val (initial, groups) = entry
      ((0 until 5).flatMap(_ => initial :+ None).toList.init, (0 until 5).flatMap(_ => groups).toList)
    }

    totalPermutations(entries.map(unfoldEntry))
  }
}
