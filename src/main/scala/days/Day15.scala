package days

import utility.*

class Day15 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val strings: Iterable[String] = input.strip.split(",")
    (part1(strings), part2(strings))
  }

  def part1(strings: Iterable[String]): Int = strings.map(calculateHashCode).sum

  def part2(strings: Iterable[String]): Int = {
    val boxes: collection.mutable.Map[Int, List[(String, Int)]] = collection.mutable.Map.empty

    def processLens(lensString: String): Unit = {
      val label = lensString.takeWhile(_.isLetter)
      val hash = calculateHashCode(label)
      val list = boxes.getOrElse(hash, List.empty)

      if (lensString.endsWith("-")) {
        if (label + "-" != lensString) throw new Exception("Lens string does not match")

        val updated = list.filterNot(_._1 == label)
        boxes.put(hash, updated)
      } else {
        val lensStrength = lensString.last.asDigit

        if (label + "=" + lensStrength != lensString) throw new Exception("Lens string does not match")

        if (list.exists(_._1 == label)) {
          val updated = list.updated(list.indexWhere(_._1 == label), (label, lensStrength))
          boxes.put(hash, updated)
        } else {
          val updated = list :+ (label, lensStrength)
          boxes.put(hash, list :+ (label, lensStrength))
        }
      }
    }

    strings.foreach(processLens)

    def calcBoxFocusingPower(hash: Int): Int = {
      boxes(hash).zipWithIndex.map({
        case ((label, strength), index) => (hash + 1) * (index + 1) * strength
      }).sum
    }

    boxes.keys.toSeq.map(calcBoxFocusingPower).sum
  }

  def getAsciiValue(c: Char): Int = c.toInt

  def calculateHashCode(s: String): Int = {
    def addCharToHash(hash: Int, c: Char): Int = {
      ((hash + getAsciiValue(c)) * 17) % 256
    }

    s.foldLeft(0)(addCharToHash)
  }
}
