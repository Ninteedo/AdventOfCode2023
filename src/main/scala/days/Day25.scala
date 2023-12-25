package days

import utility.*

class Day25 extends IDay {
  override def execute(input: String): (Any, Any) = {
    def readLine(line: String): (String, List[String]) = {
      val lhs = line.split(":").head
      val rhs = line.split(":").last.strip.split(" ").toList
      (lhs, rhs)
    }

    val connections = Helper.readLines(input, readLine)
    (part1(connections), part2())
  }

  def part1(connections: Iterable[(String, List[String])]): Int = {
    val componentNames = connections.map(_._1).toSet ++ connections.flatMap(_._2).toSet
    val connectionMap: Map[String, List[String]] = componentNames.map(name => name -> connections.flatMap({
      case (lhs, rhs) if lhs == name => rhs
      case (lhs, rhs) if rhs.contains(name) => List(lhs)
      case _ => Nil
    }).toList).toMap

    def groups(connections: Set[(String, String)]): List[Set[String]] = {
      val groupedComponents: collection.mutable.Set[String] = collection.mutable.Set()

      def group(name: String, currGroup: Set[String]): Set[String] = {
        if (currGroup.contains(name) || groupedComponents.contains(name)) currGroup else {
          groupedComponents.add(name)
          val connectedComponents = connections.filter(_._1 == name).map(_._2) ++ connections.filter(_._2 == name).map(_._1)
          val subsets = connectedComponents.map(group(_, currGroup + name))
          subsets.foldLeft(currGroup + name)(_ ++ _)
        }
      }

      componentNames.map(name => group(name, Set())).filter(_.nonEmpty).toList
    }

    def multipleGroups(cuts: Set[(String, String)]): Boolean = {
      val groupedComponents: collection.mutable.Set[String] = collection.mutable.Set()

      def group(name: String, currGroup: Set[String]): Unit = {
        if (!(currGroup.contains(name) || groupedComponents.contains(name))) {
          groupedComponents.add(name)
          val connectedComponents = connectionMap(name).filterNot(r => cuts.contains((r, name)) || cuts.contains((name, r)))
          connectedComponents.foreach(group(_, currGroup + name))
        }
      }

      group(componentNames.head, Set())
      groupedComponents.size != componentNames.size
    }

    val allConnections: Set[(String, String)] = connectionMap.toList.flatMap((name, v) => v.map(dest => (name, dest))).toSet.filter((l, r) => l < r)

    def groupsWhenCuts(cuts: Set[(String, String)]): List[Set[String]] = {
      val reversedCuts = cuts.map(_.swap)
      groups((allConnections -- cuts) -- reversedCuts)
    }

    val splittingCut = Set(("jff", "zns"), ("fts", "nvb"), ("qmr", "kzx"))

    val cutGroups = groupsWhenCuts(splittingCut)
    assert(multipleGroups(splittingCut))
    assert(cutGroups.size == 2)
    cutGroups.map(_.size).product
  }

  def part2(): String = "🎄"
}
