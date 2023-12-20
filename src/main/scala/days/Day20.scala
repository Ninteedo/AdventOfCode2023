package days

import utility.*
import utility.Helper.lcm

class Day20 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val modules: List[(String, Module)] = Helper.readLines(input, Module.read).toList
    val updated: Map[String, Module] = modules.toMap.map((name, module) => module match {
      case c: Conjunction => (name, c.addParents(name, modules.toMap))
      case _ => (name, module)
    })
    (part1(updated), part2(updated))
  }

  def part1(modules: Map[String, Module]) = {
    val pulseQueue = collection.mutable.Queue[(String, String, Pulse)]()
    var lowPulseCount, highPulseCount = 0L
    val env: collection.mutable.Map[String, Module] = collection.mutable.Map(modules.toSeq: _*)

    for (i <- 0L until 1000L) {
      pulseQueue.enqueue(("button", "broadcaster", Pulse.Low))
      while (pulseQueue.nonEmpty) {
        val (from, name, pulse) = pulseQueue.dequeue()
        if (env.contains(name)) {
          val (module, newPulses) = env(name).processPulse(pulse, from)
          env.put(name, module)
          val properPulses = newPulses.map((nextName, nextPulse) => (name, nextName, nextPulse))
          pulseQueue.enqueueAll(properPulses)
        }
        if (pulse == Pulse.Low) {
          lowPulseCount += 1
        } else {
          highPulseCount += 1
        }
      }
    }

    lowPulseCount * highPulseCount
  }

  def part2(modules: Map[String, Module]): Long = {
    val goal = "rx"

    val pulseQueue = collection.mutable.Queue[(String, String, Pulse)]()
    val env: collection.mutable.Map[String, Module] = collection.mutable.Map(modules.toSeq: _*)
    val goalSource = modules.filter(_._2.children.contains(goal)).keys.head
    val conjs = modules.filter(_._2.children.contains(goalSource)).keys.toList
    val conjCycleCache: collection.mutable.Map[String, List[Long]] = collection.mutable.Map(conjs.map((_, List())): _*)

    var cycle = 0L
    while (true) {
      cycle += 1
      pulseQueue.enqueue(("button", "broadcaster", Pulse.Low))
      while (pulseQueue.nonEmpty) {
        val (from, name, pulse) = pulseQueue.dequeue()
        if (env.contains(name)) {
          val (module, newPulses) = env(name).processPulse(pulse, from)
          env.put(name, module)
          val properPulses = newPulses.map((nextName, nextPulse) => (name, nextName, nextPulse))
          pulseQueue.enqueueAll(properPulses)

          if (conjs.contains(name) && properPulses.head._3 == Pulse.High) {
            conjCycleCache.put(name, conjCycleCache(name) :+ cycle)
            if (conjCycleCache.values.forall(_.nonEmpty)) return conjCycleCache.values.map(_.head).foldLeft(1L)(lcm)
          }
        }
      }
    }
    throw new Exception("Should not reach here")
  }

  enum Pulse {
    case High, Low
  }

  object Pulse {
    def fromBoolean(b: Boolean): Pulse = if (b) Pulse.High else Pulse.Low
  }

  abstract class Module {
    def processPulse(pulse: Pulse, from: String): (Module, List[(String, Pulse)])

    def closure(env: Map[String, Module]): List[String] = {
      children.foldLeft(children)((acc, name) => acc ++ env(name).closure(env))
    }

    val children: List[String]
  }

  object Module {
    def read(line: String): (String, Module) = {
      val pattern = """(broadcaster|button|([%&])(\w+)) -> ([\w, ]+)""".r

      def readChildren(children: String): List[String] = children.split(", ").toList

      val m = pattern.findFirstMatchIn(line).getOrElse(throw new Exception(s"Invalid line: $line"))
      val children = readChildren(m.group(4))
      if (m.group(1) == "broadcaster") {
        (m.group(1), Broadcaster(children))
      } else {
        (m.group(3), m.group(2) match {
          case "%" => FlipFlop(children)
          case "&" => Conjunction(children)
        })
      }
    }
  }

  case class Broadcaster(children: List[String]) extends Module {
    override def processPulse(pulse: Pulse, from: String): (Module, List[(String, Pulse)]) = {
      (this, children.zip(LazyList.continually(pulse)))
    }
  }

  case class FlipFlop(state: Boolean, children: List[String]) extends Module {
    override def processPulse(pulse: Pulse, from: String): (Module, List[(String, Pulse)]) = pulse match {
      case Pulse.High => (this, Nil)
      case Pulse.Low => (FlipFlop(!state, children), children.zip(LazyList.continually(Pulse.fromBoolean(!state))))
    }
  }

  object FlipFlop {
    def apply(children: List[String]): FlipFlop = FlipFlop(false, children)
  }

  case class Conjunction(state: Map[String, Pulse], children: List[String]) extends Module {
    override def processPulse(pulse: Pulse, from: String): (Module, List[(String, Pulse)]) = {
      val newState = state + (from -> pulse)
      val newModule = Conjunction(newState, children)
      if (newState.values.forall(_ == Pulse.High)) {
        (newModule, children.zip(LazyList.continually(Pulse.Low)))
      } else {
        (newModule, children.zip(LazyList.continually(Pulse.High)))
      }
    }

    def addParents(moduleName: String, env: Map[String, Module]): Conjunction = {
      val parents = env.filter(_._2.children.contains(moduleName)).keys.toList
      Conjunction(parents.zip(LazyList.continually(Pulse.Low)).toMap, children)
    }
  }

  object Conjunction {
    def apply(children: List[String]): Conjunction = {
      Conjunction(Map(), children)
    }
  }
}

// not 0
// 1297304298640 too low
// 1297304298641 too low
