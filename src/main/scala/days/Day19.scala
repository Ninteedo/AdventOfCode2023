package days

import utility.*

class Day19 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val sections = input.split("\n\n")
    val partsList = sections.last.split("\n").map(Part.read).toList
    val workflowsList = sections.head.split("\n").map(Workflow.read)
    val workflowsMap = workflowsList.map(workflow => workflow.name -> workflow).toMap

    (part1(workflowsMap, partsList), part2(workflowsMap, partsList))
  }

  def part1(workflows: Map[String, Workflow], parts: List[Part]): Long = {
    val in = workflows("in")
    parts.filter(in.execute(_, workflows)).map(_.value).sum
  }

  def part2(workflows: Map[String, Workflow], parts: List[Part]): Long = {
    calculateValidParts(workflows, "in", 4000L)
  }

  case class Part(x: Long, m: Long, a: Long, s: Long) {
    def subPart(kind: Char): Long = kind match {
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s
    }

    def copyAndReplace(kind: Char, value: Long): Part = kind match {
      case 'x' => Part(value, m, a, s)
      case 'm' => Part(x, value, a, s)
      case 'a' => Part(x, m, value, s)
      case 's' => Part(x, m, a, value)
    }

    val value: Long = x + m + a + s
  }

  object Part {
    def read(s: String): Part = {
      val removedBraces = s.stripPrefix("{").stripSuffix("}")
      val splits = removedBraces.split(",")

      val values = kinds.map(kind => splits.find(_.startsWith(s"$kind=")).map(_.stripPrefix(s"$kind=")).get.toLong)
      Part(values(0), values(1), values(2), values(3))
    }

    def all(value: Long): Part = Part(value, value, value, value)
  }

  val kinds: List[Char] = List('x', 'm', 'a', 's')

  case class Workflow(name: String, rules: List[FlowRule], elseRule: ElseRule) {
    def execute(input: Part, workflows: Map[String, Workflow]): Boolean = {
      val ruleSuccess: Option[Rule] = rules.find(_.execute(input).isDefined)
      ruleSuccess.getOrElse(elseRule).execute(input).get match {
        case Left(result) => result
        case Right(otherName) => workflows(otherName).execute(input, workflows)
      }
    }
  }

  object Workflow {
    def read(s: String): Workflow = {
      val name = s.takeWhile(_ != '{')
      val removedBraces = s.drop(name.length + 1).stripSuffix("}")
      val splits = removedBraces.split(",")

      def makeResultEither(result: String): Either[Boolean, String] = result match {
        case "A" => Left(true)
        case "R" => Left(false)
        case other => Right(other)
      }

      val pattern = """([xmas])([><])(\d+):([a-zAR]+)""".r
      val rules = splits.init.map(split => {
        val pattern(kind, operator, valueString, result) = split
        val resultEither = makeResultEither(result)
        FlowRule(kind.head, operator.head, valueString.toLong, resultEither)
      })

      val elseRule = ElseRule(makeResultEither(splits.last))

      Workflow(name, rules.toList, elseRule)
    }
  }

  abstract class Rule {
    def execute(part: Part): Option[Either[Boolean, String]]
  }

  case class FlowRule(kind: Char, operator: Char, value: Long, result: Either[Boolean, String]) extends Rule {
    override def execute(part: Part): Option[Either[Boolean, String]] = {
      operator match {
        case '>' => if (part.subPart(kind) > value) Some(result) else None
        case '<' => if (part.subPart(kind) < value) Some(result) else None
      }
    }
  }

  case class ElseRule(result: Either[Boolean, String]) extends Rule {
    override def execute(part: Part): Option[Either[Boolean, String]] = Some(result)
  }

  def calculateValidParts(workflows: Map[String, Workflow], start: String, maxSubpartCount: Long): Long = {
    def countValidRecursive(ruleResult: Either[Boolean, String], partMin: Part, partMax: Part): Long = {
      def calculateOccupied(l: Part, r: Part): Long = {
        kinds.map(kind => partMax.subPart(kind) - partMin.subPart(kind) + 1).product
      }

      var remaining = calculateOccupied(partMin, partMax)
      ruleResult match {
        case Left(result) => if (result) remaining else 0L
        case Right(result) => {
          val workflow = workflows(result)
          var (currentPartMin, currentPartMax) = (partMin, partMax)

          def processRule(rule: Rule): Long = rule match {
            case ElseRule(result) => countValidRecursive(result, currentPartMin, currentPartMax)
            case FlowRule(kind, operator, value, result) => {
              val (thisPartMin, thisPartMax) = operator match {
                case '>' => {
                  val res = (currentPartMin.copyAndReplace(kind, value + 1), currentPartMax)
                  currentPartMax = currentPartMax.copyAndReplace(kind, value)
                  res
                }
                case '<' => {
                  val res = (currentPartMin, currentPartMax.copyAndReplace(kind, value - 1))
                  currentPartMin = currentPartMin.copyAndReplace(kind, value)
                  res
                }
              }

              val occupied = calculateOccupied(thisPartMin, thisPartMax)
              remaining -= occupied

              countValidRecursive(result, thisPartMin, thisPartMax)
            }
          }

          (workflow.rules :+ workflow.elseRule).map(processRule).sum
        }
      }
    }

    countValidRecursive(Right(start), Part.all(1), Part.all(maxSubpartCount))
  }
}
