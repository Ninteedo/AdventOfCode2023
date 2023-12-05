package days

import utility.*

class Day05 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val groups: Seq[String] = input.split("\n\n")
    val seedNumbers: Seq[BigInt] = groups.head.stripPrefix("seeds: ").split(" ").map(s => BigInt(s.strip, 10)).toSeq
    val almanacEntries: Seq[AlmanacMapping] = groups.tail.map(AlmanacMapping.read)

    (part1(seedNumbers, almanacEntries), part2(seedNumbers, almanacEntries))
  }

  def part1(seedNumbers: Seq[BigInt], almanacEntries: Seq[AlmanacMapping]): BigInt = {
    def seedNumberToLocation(seedNumber: BigInt): BigInt =
      almanacEntries.foldLeft(seedNumber)({(n: BigInt, almanac: AlmanacMapping) => almanac.getMappedValue(n)})

    seedNumbers.map(seedNumberToLocation).min
  }

  def part2(seedNumbers: Seq[BigInt], almanacEntries: Seq[AlmanacMapping]): BigInt = {
    def seedRangeToLocationRange(seedRange: (BigInt, BigInt)): Seq[(BigInt, BigInt)] =
      almanacEntries.foldLeft(Seq(seedRange))({(range, almanac) => almanac.transformSeedRanges(range)})

    val initialSeedRanges: Seq[(BigInt, BigInt)] =
      (0 until seedNumbers.length / 2).map(i => (seedNumbers(2 * i), seedNumbers(2 * i + 1)))

    initialSeedRanges.flatMap(seedRangeToLocationRange).map(_._1).min
  }

  class AlmanacMapping(
    val sourceName: String,
    val destName: String,
    val mappings: Seq[(BigInt, BigInt, BigInt)]
  ) {
    def getMappedValue(sourceValue: BigInt): BigInt = mappings.find((destStart, sourceStart, rangeLength) => {
      sourceStart <= sourceValue && sourceValue < sourceStart + rangeLength
    }) match {
      case Some((destStart, sourceStart, rangeLength)) => sourceValue + (destStart - sourceStart)
      case None => sourceValue
    }

    def transformSeedRanges(seedRanges: Seq[(BigInt, BigInt)]): Seq[(BigInt, BigInt)] = {
      def transformSeedRange(start: BigInt, length: BigInt): Seq[(BigInt, BigInt)] = {
        def transformInMapping(mapping: (BigInt, BigInt, BigInt)): (BigInt, BigInt) = {
          val (destStart, sourceStart, rangeLength) = mapping

          val amountInMapping: BigInt = {
            val overlapStart = start.max(sourceStart)
            val overlapEnd = (start + length).min(sourceStart + rangeLength)
            (overlapEnd - overlapStart).max(0)
          }

          (start.max(sourceStart), amountInMapping)
        }
        val converted: Seq[(BigInt, BigInt)] = mappings.map(transformInMapping).filter(_._2 > 0)

        var remainingRange: Seq[(BigInt, BigInt)] = List((start, start + length))  // find existing range not mapped
        converted.foreach({ m =>
          val (origStart, length) = m
          remainingRange = remainingRange.flatMap({n =>
            val (remainingStart, remainingEnd) = n
            val before = (remainingStart, origStart.min(remainingEnd))
            val after = (origStart + length, remainingEnd)
            List(before, after)
          }).filter({p => p._1 < p._2})
        })

        converted.map(p => (getMappedValue(p._1), p._2)) ++ remainingRange.map(p => (p._1, p._2 - p._1))
      }

      seedRanges.flatMap(transformSeedRange)
    }
  }

  object AlmanacMapping {
    def read(s: String): AlmanacMapping = {
      val lines: Seq[String] = Helper.readLines(s, identity).toSeq
      val headerLine = lines.head
      val (sourceName, destName) = "(\\w+)-to-(\\w+) map:".r.findFirstMatchIn(headerLine) match {
        case Some(value) => (value.group(1), value.group(2))
      }

      val mappingPattern = "(\\d+) (\\d+) (\\d+)".r
      val mappings: Seq[(BigInt, BigInt, BigInt)] = lines.tail.map(line => mappingPattern.findFirstMatchIn(line) match {
        case Some(value) => (BigInt(value.group(1), 10), BigInt(value.group(2), 10), BigInt(value.group(3), 10))
      })

      AlmanacMapping(sourceName, destName, mappings)
    }
  }
}
