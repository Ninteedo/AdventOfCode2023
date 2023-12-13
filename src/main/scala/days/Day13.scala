package days

import utility.*

class Day13 extends IDay {
  type Pattern = Grid2D[Boolean]

  override def execute(input: String): (Int, Int) = {
    def parsePattern(pattern: String): Pattern = {
      def parseChar(c: Char): Boolean = c match {
        case '.' => false
        case '#' => true
        case _ => throw new Exception(s"Invalid character '$c'")
      }
      Grid2D.from2DCharArray(pattern, parseChar)
    }

    val patterns = input.split("\n\n").map(parsePattern)
    (part1(patterns), part2(patterns))
  }

  def part1: Iterable[Pattern] => Int = totalPatternsValue(false)

  def part2: Iterable[Pattern] => Int = totalPatternsValue(true)

  def totalPatternsValue(smudges: Boolean)(patterns: Iterable[Pattern]): Int = {
    def calcPatternValue(smudges: Boolean)(pattern: Pattern): Int = {
      def findReflectionIndex(transpose: Boolean): Option[Int] = {
        val p: Pattern = if (transpose) pattern.transpose else pattern
        (1 until p.colCount).find(reflectIndex => {
          var smudgeUsed: Boolean = !smudges
          (0 until reflectIndex).forall(x => {
            val otherX: Int = reflectIndex + (reflectIndex - x) - 1
            !p.contains(Point2D(otherX, 0)) || (0 until p.rowCount).forall(y => {
              p.at(Point2D(x, y)) == p.at(Point2D(otherX, y)) || {
                val res = !smudgeUsed
                smudgeUsed = true
                res
              }
            })
          }) && smudgeUsed
        })
      }

      findReflectionIndex(false).getOrElse(
        100 * findReflectionIndex(true).getOrElse(
          throw new Exception("No reflection found")
        )
      )
    }

    patterns.map(calcPatternValue(smudges)).sum
  }
}
