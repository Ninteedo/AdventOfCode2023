package utility

import scala.collection.mutable

trait SearchNode[T <: SearchNode[T]] {
  lazy val orderingValue: Int = {
    val result: Int = calculateOrderingValue
    if (strict && getParent.isDefined && result > getParent.get.orderingValue)
      sys.error(s"invalid child ordering value $result, parent has ${getParent.get.orderingValue}\n$pathString")
    result
  }

  def calculateOrderingValue: Int

  def descendents: Iterable[T]

  lazy val atGoal: Boolean

  lazy val getParent: Option[T]

  lazy val getResult: Int

  def isDuplicateOf(other: SearchNode[T]): Boolean = orderingValue == other.orderingValue

  override def equals(obj: Any): Boolean = obj match {
    case otherNode: SearchNode[T] => isDuplicateOf(otherNode)
    case _ => false
  }

  override def hashCode(): Int = orderingValue

  val filterDuplicates: Boolean = false

  val strict: Boolean = true

  def bestFirstSearch(): Option[SearchNode[T]] = {
    val frontier: mutable.PriorityQueue[SearchNode[T]] = mutable.PriorityQueue(this)(Ordering.by(_.orderingValue))
    val visited = mutable.Set[SearchNode[T]]()

    while (frontier.nonEmpty) {
      val node: SearchNode[T] = frontier.dequeue()
      if (node.atGoal) return Some(node)

      if (!(filterDuplicates && visited.contains(node))) {
        var nodesToAdd = node.descendents
        if (filterDuplicates) {
          visited.add(node)
          nodesToAdd = nodesToAdd.filterNot(visited.contains(_))
        }
        nodesToAdd.foreach(frontier.enqueue(_))
      }
    }

    None
  }

  lazy val pathString: String = {
    var result: List[String] = List()
    var curr: Option[SearchNode[T]] = Some(this)
    while (curr.isDefined) {
      result = curr.get.toString :: result
      curr = curr.get.getParent
    }
    result.mkString("\n")
  }

  override def toString: String = {
    s"SearchNode(order=$calculateOrderingValue, result=$getResult)"
  }
}
