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

  def part1(connections: Iterable[(String, List[String])]) = {
    val componentNames = connections.map(_._1).toSet ++ connections.flatMap(_._2).toSet
    val connectionMap: Map[String, List[String]] = componentNames.map(name => name -> connections.flatMap({
      case (lhs, rhs) if lhs == name => rhs
      case (lhs, rhs) if rhs.contains(name) => List(lhs)
      case _ => Nil
    }).toList).toMap
//    println(componentNames.mkString("\n"))
//    println(connectionMap.flatMap((name, v) => v.map(dest => s"$name $dest")).mkString("\n"))

    def condense(components: Set[String], connections: Set[(String, String)]): (Set[String], Set[(String, String)]) = {
      def lengthTwoConnections(a: String, b: String): Int = {
        val aConnections = connections.filter(_._1 == a).map(_._2) ++ connections.filter(_._2 == a).map(_._1)
        val bConnections = connections.filter(_._1 == b).map(_._2) ++ connections.filter(_._2 == b).map(_._1)
        aConnections.intersect(bConnections).size
      }

      components.toList.combinations(2).find(pair => lengthTwoConnections(pair.head, pair.last) > 2) match {
        case Some(toCondense) => {
          val newComponent = toCondense.mkString("-")
          val newConnections = (connections
            ++ toCondense.flatMap(name => connections.filter(_._1 == name).map(c => (newComponent, c._2)))
            ++ toCondense.flatMap(name => connections.filter(_._2 == name).map(c => (newComponent, c._1))))
            .filterNot(r => toCondense.contains(r._1) || toCondense.contains(r._2))
          condense(components - toCondense.head - toCondense.last + newComponent, newConnections)
        }
        case None => (components, connections)
      }
    }

    val (condensedComponents, condensedConnections) = condense(
      componentNames, connectionMap.toList.flatMap((name, v) => v.map(dest => (name, dest))).toSet
    )
    println(condensedComponents.mkString("\n"))
    println(condensedConnections.mkString("\n"))
    val condensedConnectionMap: Map[String, List[String]] = condensedComponents
      .map(name => name -> condensedConnections.flatMap({
        case (lhs, rhs) if lhs == name => List(rhs)
        case (lhs, rhs) if rhs == name => List(lhs)
        case _ => Nil
      }).toList
      ).toMap

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
          val connectedComponents = condensedConnectionMap(name).filterNot(r => cuts.contains(r, name) || cuts.contains(name, r))
          connectedComponents.foreach(group(_, currGroup + name))
        }
      }

      group(condensedComponents.head, Set())
      groupedComponents.size < condensedComponents.size - 1
    }

    val allConnections: Set[(String, String)] = condensedConnections // connectionMap.toList.flatMap((name, v) => v.map(dest => (name, dest))).toSet.filter((l, r) => l < r)

    def groupsWhenCuts(cuts: Set[(String, String)]): List[Set[String]] = {
      val reversedCuts = cuts.map(_.swap)
      groups((allConnections -- cuts) -- reversedCuts)
    }

    val splittingCut: Set[(String, String)] = allConnections.toList.combinations(3).find(cuts => {
      println(cuts)
      multipleGroups(cuts.toSet)
    }).get.toSet

    groupsWhenCuts(splittingCut).map(_.flatMap(_.split("-")).size).product
  }

  def part2() = {
    incomplete
  }
}
