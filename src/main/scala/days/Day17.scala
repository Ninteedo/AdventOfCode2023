package days

import utility.*

class Day17 extends IDay {
  type City = Grid2D[Int]

  override def execute(input: String): (Int, Int) = {
    val city: City = Grid2D.from2DCharArray(input, _.asDigit)
    (part1(city), part2(city))
  }

  def part1(city: City): Int = CityBlockNode(Nil, Point2D.zero, 0, None, 0, city).bestFirstSearch().get.getResult

  def part2(city: City): Int = UltraCityBlockNode(Nil, Point2D.zero, 0, None, 0, city).bestFirstSearch().get.getResult

  class CityBlockNode(val route: List[CityBlockNode], val pos: Point2D, val loss: Int,
                      val lastDir: Option[Point2D], val straightLineDist: Int, val city: City)
    extends SearchNode[CityBlockNode] {

    override lazy val getParent: Option[CityBlockNode] = route.lastOption

    override def calculateOrderingValue: Int = -getResult

    override val filterDuplicates: Boolean = true

    val minStraightDist: Int = 1

    val maxStraightDist: Int = 3

    override def descendents: Iterable[CityBlockNode] = {
      var dirs = Point2D.directions
      if (lastDir.isDefined) {
        dirs = Point2D.directions.filter(_ != lastDir.get * -1)
        if (straightLineDist < minStraightDist) {
          dirs = LazyList(lastDir.get)
        } else if (straightLineDist >= maxStraightDist) {
          dirs = dirs.filter(d => d != lastDir.get && d != lastDir.get * -1)
        }
      }
      dirs = dirs.filter(dir => city.contains(pos + dir))
      val minRouteDist = route.map(_.pos.mannDist(goalPos)).minOption.getOrElse(Point2D.zero.mannDist(goalPos))
      val positions = dirs.map(pos + _).filterNot(route.map(_.pos).contains)
      positions.map(createNode)
    }

    def nextStraightLineDistance(newPos: Point2D): Int = if (lastDir.contains(newPos - pos)) straightLineDist + 1 else 1

    def createNode(newPos: Point2D): CityBlockNode = new CityBlockNode(
      route :+ this,
      newPos,
      loss + city.at(newPos),
      Some(newPos - pos),
      nextStraightLineDistance(newPos),
      city
    )

    override lazy val getResult: Int = loss

    override def isDuplicateOf(other: SearchNode[CityBlockNode]): Boolean = other match {
      case other: CityBlockNode => other.pos == pos && other.lastDir == lastDir && other.straightLineDist == straightLineDist
    }

    override lazy val atGoal: Boolean = pos == goalPos

    override def hashCode(): Int = (pos.hashCode() * 31 + lastDir.hashCode()) * 31 + straightLineDist

    val goalPos: Point2D = Point2D(city.colCount - 1, city.rowCount - 1)

    override def toString: String = s"CityBlockNode($pos, dir=$lastDir, loss=$loss, order=$calculateOrderingValue)"
  }

  class UltraCityBlockNode(route: List[UltraCityBlockNode], pos: Point2D, loss: Int,
                           lastDir: Option[Point2D], straightLineDist: Int, city: City)
    extends CityBlockNode(route, pos, loss, lastDir, straightLineDist, city) {

    override val minStraightDist: Int = 4

    override val maxStraightDist: Int = 10

    override def createNode(newPos: Point2D): UltraCityBlockNode = UltraCityBlockNode(
      route :+ this,
      newPos,
      loss + city.at(newPos),
      Some(newPos - pos),
      nextStraightLineDistance(newPos),
      city
    )
  }
}
