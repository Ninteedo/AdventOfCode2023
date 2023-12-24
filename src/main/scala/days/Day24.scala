package days

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import utility.*

class Day24 extends IDay {
  override def execute(input: String): (Int, Long) = {
    val hailstones: Iterable[Hailstone] = Helper.readLines(input, readHailstone)
    (part1(hailstones), part2(hailstones.toList))
  }

  def part1(hailstones: Iterable[Hailstone]): Int = {
    hailstones.toList.combinations(2).count({ case List(a, b) => a.intersectsInRangeIgnoreZ(b)})
  }

  def part2(hailstones: List[Hailstone]): Long = {
    val i = hailstones(0)
    val j = hailstones(1)
    val k = hailstones(2)

    val p0 = DenseVector(i.px, i.py, i.pz).map(_.toDouble)
    val p1 = DenseVector(j.px, j.py, j.pz).map(_.toDouble)
    val p2 = DenseVector(k.px, k.py, k.pz).map(_.toDouble)
    val v0 = DenseVector(i.vx, i.vy, i.vz).map(_.toDouble)
    val v1 = DenseVector(j.vx, j.vy, j.vz).map(_.toDouble)
    val v2 = DenseVector(k.vx, k.vy, k.vz).map(_.toDouble)

    val matrix = DenseMatrix.zeros[Double](6, 6)
    matrix(0 to 2, 0 to 2) := crossMatrix(v0) - crossMatrix(v1)
    matrix(0 to 2, 3 to 5) := crossMatrix(p1) - crossMatrix(p0)
    matrix(3 to 5, 0 to 2) := crossMatrix(v0) - crossMatrix(v2)
    matrix(3 to 5, 3 to 5) := crossMatrix(p2) - crossMatrix(p0)
    val vector = DenseVector.vertcat(
      crossProduct(p1, v1) - crossProduct(p0, v0),
      crossProduct(p2, v2) - crossProduct(p0, v0)
    )
    val inverseMatrix = inv(matrix)

    val resultVector = inverseMatrix * vector
    val rounded = resultVector.map(_.round)
    rounded(0) + rounded(1) + rounded(2)
  }

  case class Hailstone(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long) {
    def intersectsInRangeIgnoreZ(other: Hailstone): Boolean = {
      def inRange(value: Double) = 200000000000000L <= value && value <= 400000000000000L

      val line1 = Line(px, py, vx, vy)
      val line2 = Line(other.px, other.py, other.vx, other.vy)
      line1.getIntersection(line2) match {
        case Some((x, y)) => {
          val t1 = (x - px) / vx
          val t2 = (x - other.px) / other.vx
          inRange(x) && inRange(y) && t1 >= 0 && t2 >= 0
        }
        case None => false
      }

    }
  }

  def readHailstone(line: String): Hailstone = {
    val pattern = """(-?\d+),\s*(-?\d+),\s*(-?\d+)\s*@\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)""".r

    line match {
      case pattern(x, y, z, vx, vy, vz) => Hailstone(x.toLong, y.toLong, z.toLong, vx.toLong, vy.toLong, vz.toLong)
    }
  }

  case class Line(px: Double, py: Double, vx: Double, vy: Double) {
    val m: Double = vy / vx
    val c: Double = py - m * px

    def getIntersection(other: Line): Option[(Double, Double)] = {
      val o = other
      if (m == o.m) None else {
        val t = ((o.px - px) * o.vy - (o.py - py) * o.vx) / (vx * o.vy - vy * o.vx)
        Some((px + vx * t, py + vy * t))
      }
    }
  }

  def crossProduct(u: DenseVector[Double], v: DenseVector[Double]): DenseVector[Double] = DenseVector(
    u(1) * v(2) - u(2) * v(1),
    u(2) * v(0) - u(0) * v(2),
    u(0) * v(1) - u(1) * v(0)
  )

  def crossMatrix(v: DenseVector[Double]): DenseMatrix[Double] = DenseMatrix(
    (0.0, -v(2), v(1)),
    (v(2), 0.0, -v(0)),
    (-v(1), v(0), 0.0)
  )
}
