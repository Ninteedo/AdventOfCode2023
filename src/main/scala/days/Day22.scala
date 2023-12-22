package days

import utility.*

class Day22 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val bricks: Array[Brick] = Helper.readLines(input, Brick.read).toArray
    val sim = Simulation.createFromBricks(bricks).stabilise
    (part1(sim), part2(sim))
  }

  def part1(sim: Simulation): Int = {
    val supportedByOne: Map[Int, Int] = sim.supported.keys
      .filter(i => sim.supported(i).length == 1 && !sim.bricks(i).resting)
      .map(i => i -> sim.supported(i).head).toMap
    sim.bricks.indices.count(i => supportedByOne.values.forall(_ != i))
  }

  def part2(sim: Simulation): Int = {
    def totalSupported(i: Int): Int = {
      val queue: collection.mutable.Queue[Int] = collection.mutable.Queue(i)
      val removed: collection.mutable.Set[Int] = collection.mutable.Set()

      while (queue.nonEmpty) {
        val index = queue.dequeue()
        if (!removed.contains(index)) {
          removed.add(index)
          sim.supports(index).foreach(j => if (!removed.contains(j) && sim.supported(j).forall(removed.contains)) queue.enqueue(j))
        }
      }

      removed.size - 1
    }

    sim.bricks.indices.map(totalSupported).sum
  }

  case class Brick(a: Point3D, b: Point3D) {
    val bottom: Int = a.z.min(b.z)
    val top: Int = a.z.max(b.z)

    def fall(distance: Int): Brick = Brick(a - Point3D(0, 0, distance), b - Point3D(0, 0, distance))

    def resting: Boolean = bottom == 1

    def collidingIgnoreZ(other: Brick): Boolean = {
      lazy val xColliding = overlappingRanges((a.x, b.x), (other.a.x, other.b.x))
      lazy val yColliding = overlappingRanges((a.y, b.y), (other.a.y, other.b.y))
      xColliding && yColliding
    }

    def supporting(other: Brick): Boolean = collidingIgnoreZ(other) && top + 1 == other.bottom

    def above(other: Brick): Boolean = bottom > other.top

    def heightDifference(other: Brick): Int = bottom - other.top - 1
  }

  object Brick {
    def read(line: String): Brick = {
      def readPoint(s: String) = {
        val parts = s.split(",").map(_.toInt)
        Point3D(parts(0), parts(1), parts(2))
      }

      Brick(readPoint(line.split("~").head), readPoint(line.split("~").last))
    }
  }

  case class Simulation(bricks: Array[Brick], collidable: Map[Int, List[Int]]) {
    def stabilise: Simulation = {
      def moveBrickDown(otherBricks: Array[Brick], index: Int): Brick = {
        val brick = bricks(index)
        val fallDistance = collidable(index).map(bricks) match {
          case Nil => brick.bottom - 1
          case others => others.map(brick.heightDifference).min
        }
        brick.fall(fallDistance)
      }

      val newBricks = bricks.indices.map(i => moveBrickDown(bricks, i))
      val res = Simulation(newBricks.toArray, collidable)
      if (!(res.bricks sameElements bricks)) res.stabilise else res
    }

    lazy val supported: Map[Int, Array[Int]] = bricks.indices
      .map(i => i -> bricks.indices.filter(j => bricks(j).supporting(bricks(i))).toArray).toMap

    lazy val supports: Map[Int, Array[Int]] = bricks.indices
      .map(i => i -> bricks.indices.filter(j => bricks(i).supporting(bricks(j))).toArray).toMap
  }

  object Simulation {
    def createFromBricks(bricks: Array[Brick]): Simulation = {
      val sortedBricks = bricks.sortBy(brick => brick.a.z.min(brick.b.z))
      val collidable: Map[Int, List[Int]] = sortedBricks.indices.map(i =>
        i -> sortedBricks.indices.filter(j =>
          j != i && sortedBricks(i).above(sortedBricks(j)) && sortedBricks(i).collidingIgnoreZ(sortedBricks(j))
        ).toList
      ).toMap
      Simulation(sortedBricks, collidable)
    }
  }

  def overlappingRanges(a: (Int, Int), b: (Int, Int)): Boolean = {
    !(a._1.max(a._2) < b._1.min(b._2) || b._1.max(b._2) < a._1.min(a._2))
  }
}
