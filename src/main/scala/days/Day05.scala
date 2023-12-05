package days

import utility.*

class Day05 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val groups: Seq[String] = input.split("\n\n")
    val seedNumbers: Seq[Long] = groups.head.stripPrefix("seeds: ").split(" ").map(s => s.strip.toLong).toSeq
    val almanacEntries: Seq[AlmanacMapping] = groups.tail.map(AlmanacMapping.read)

    (part1(seedNumbers, almanacEntries), part2(seedNumbers, almanacEntries))
  }

  def part1(seedNumbers: Seq[Long], almanacEntries: Seq[AlmanacMapping]): Long = {
    def seedNumberToLocation(seedNumber: Long): Long =
      almanacEntries.foldLeft(seedNumber)({(n: Long, almanac: AlmanacMapping) => almanac.getMappedValue(n)})

    seedNumbers.map(seedNumberToLocation).min
  }

  def part2(seedNumbers: Seq[Long], almanacEntries: Seq[AlmanacMapping]): Long = {
    def seedRangeToLocationRange(seedRange: LongRange): Seq[LongRange] =
      almanacEntries.foldLeft(Seq(seedRange))({(range, almanac) => almanac.transformSeedRanges(range)})

    val initialSeedRanges: Seq[LongRange] =
      (0 until seedNumbers.length / 2).map(i => LongRange.fromStartAndSize(seedNumbers(2 * i), seedNumbers(2 * i + 1)))

    initialSeedRanges.flatMap(seedRangeToLocationRange).map(_.start).min
  }

  class AlmanacMapping(val mappings: Seq[(LongRange, Long)]) {
    def getMappedValue(sourceValue: Long): Long = mappings.find((range, offset) => {
      range.contains(sourceValue)
    }) match {
      case Some((range, offset)) => sourceValue + offset
      case None => sourceValue
    }

    def transformSeedRanges(seedRanges: Seq[LongRange]): Seq[LongRange] = {
      def transformSeedRange(original: LongRange): Seq[LongRange] = {
        def transformInMapping(mapping: (LongRange, Long)): LongRange = {
          val (range, _) = mapping
          val amountInMapping: Long = original.overlapAmount(range)
          LongRange.fromStartAndSize(original.start.max(range.start), amountInMapping)
        }
        val converted: Seq[LongRange] = mappings.map(transformInMapping).filter(_.nonEmpty)
        var remainingRange: Seq[LongRange] = List(original)  // find existing range not mapped
        converted.foreach({ r => remainingRange = remainingRange.flatMap(_.difference(r))})

        converted.map(p => LongRange.fromStartAndSize(getMappedValue(p.start), p.size)) ++ remainingRange
      }

      seedRanges.flatMap(transformSeedRange)
    }
  }

  object AlmanacMapping {
    def read(s: String): AlmanacMapping = {
      val lines: Seq[String] = Helper.readLines(s, identity).toSeq
      val mappingPattern = "(\\d+) (\\d+) (\\d+)".r
      val readValues: Seq[(Long, Long, Long)] = lines.tail.map(line => mappingPattern.findFirstMatchIn(line) match {
        case Some(value) => (value.group(1).toLong, value.group(2).toLong, value.group(3).toLong)
      })

      val mappings = readValues.map(t => (LongRange(t._2, t._2 + t._3), t._1 - t._2))
      AlmanacMapping(mappings)
    }
  }

  class LongRange(val start: Long, val end: Long) {
    def contains(value: Long): Boolean = start <= value && value < end

    def difference(other: LongRange): Seq[LongRange] = {
      val before = LongRange(start, end.min(other.start))
      val after = LongRange(other.end, end)
      List(before, after).filter(_.nonEmpty)
    }

    def overlapAmount(other: LongRange): Long = (end.min(other.end) - start.max(other.start)).max(0)

    def nonEmpty: Boolean = start < end

    val size: Long = (end - start).max(0L)

    override def toString: String = s"LongRange($start, $end)"
  }

  object LongRange {
    def fromStartAndSize(start: Long, size: Long): LongRange = LongRange(start, start + size)
  }
}
