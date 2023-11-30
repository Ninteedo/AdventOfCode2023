package utility

import scala.collection.mutable

trait SearchNode[T <: SearchNode[T]] {
  lazy val orderingValue: Int = {
    val result: Int = calculateOrderingValue
    if (getParent != null && result > getParent.orderingValue)
      sys.error(s"invalid child ordering value $result, parent has ${getParent.orderingValue}\n$pathString")
    result
  }

  def calculateOrderingValue: Int

  lazy val descendents: Iterable[T]

  lazy val atGoal: Boolean

  lazy val getParent: T

  lazy val getResult: Int

  def isDuplicateOf(other: SearchNode[T]): Boolean = orderingValue == other.orderingValue

  override def equals(obj: Any): Boolean = obj match {
    case otherNode: SearchNode[T] => isDuplicateOf(otherNode)
    case _ => false
  }

  override def hashCode(): Int = orderingValue

  val filterDuplicates: Boolean = false

  def bestFirstSearch(): Option[SearchNode[T]] = {
    val frontier: mutable.PriorityQueue[SearchNode[T]] = mutable.PriorityQueue(this)(Ordering.by(_.orderingValue))
    val visited = mutable.Set[SearchNode[T]]()

    while (frontier.nonEmpty) {
      val node: SearchNode[T] = frontier.dequeue()
      if (node.atGoal) return Some(node)

      var nodesToAdd = node.descendents
      if (filterDuplicates) {
        visited.add(node)
        nodesToAdd = nodesToAdd.filterNot(visited.contains(_))
      }
      nodesToAdd.foreach(frontier.enqueue(_))
    }

    None
  }

  lazy val pathString: String = {
    var result: List[String] = List()
    var curr: SearchNode[T] = this
    while (curr != null) {
      result = curr.toString :: result
      curr = curr.getParent
    }
    result.mkString("\n")
  }

  override def toString: String = {
    s"SearchNode(order=$orderingValue, result=$getResult)"
  }
}
