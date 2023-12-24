package days

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import utility.*

class Day24 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val hailstones: Iterable[Hailstone] = Helper.readLines(input, readHailstone)
    (part1(hailstones), part2(hailstones.toList))
  }

  def part1(hailstones: Iterable[Hailstone]): Int = {
    hailstones.toList.combinations(2).count({ case List(a, b) => a.intersectsInRangeIgnoreZ(b)})
  }

  def part2(hailstones: List[Hailstone]) = {
    val i = hailstones(0)
    val j = hailstones(1)
    val k = hailstones(2)

    val matrix = DenseMatrix(
      (0L, j.vz - i.vz, i.vy - j.vy, 0L, k.vz - i.vz, i.vy - k.vy),
      (i.vz - j.vz, 0L, j.vx - i.vx, i.vz - k.vz, 0L, k.vx - i.vx),
      (j.vy - i.vy, i.vx - j.vx, 0L, k.vy - i.vy, i.vx - k.vx, 0L),
      (0L, j.pz - i.pz, i.py - j.py, 0L, k.pz - i.pz, i.py - k.py),
      (i.pz - j.pz, 0L, j.px - i.px, i.pz - k.pz, 0L, k.px - i.px),
      (j.py - i.py, i.px - j.px, 0L, k.py - i.py, i.px - k.px, 0L)
    )
    val vector = DenseVector(
      (j.py * j.vz - j.pz * j.vy).toDouble - (i.py * i.vz - i.pz * i.vy),
      (j.pz * j.vx - j.px * j.vz).toDouble - (i.pz * i.vx - i.px * i.vz),
      (j.px * j.vy - j.py * j.vx).toDouble - (i.px * i.vy - i.py * i.vx),
      (k.py * k.vz - k.pz * k.vy).toDouble - (i.py * i.vz - i.pz * i.vy),
      (k.pz * k.vx - k.px * k.vz).toDouble - (i.pz * i.vx - i.px * i.vz),
      (k.px * k.vy - k.py * k.vx).toDouble - (i.px * i.vy - i.py * i.vx)
    )
    println(matrix)
    val inverseMatrix = inv(matrix) // invertMatrix(matrix.map(_.map(_.toDouble))).get
    println(inverseMatrix)

    val resultVector = inverseMatrix * vector  // inverseMatrix.transpose.map(row => row.zip(vector).map({ case (a, b) => a * b }).sum)
    println(resultVector)
  }

//  def part2(hailstones: Iterable[Hailstone]) = {
//    val n = hailstones.size * 2
//    val a = Array.ofDim[Double](n, 6)
//    val b = Array.ofDim[Double](n)
//
//    var equationIndex = 0
//    for (h <- hailstones) {
//      // Create equations based on the hailstones' positions and velocities
////      a(equationIndex)(0) = 1 // Coefficient for x0n
////      a(equationIndex)(3) = -h.vx // Coefficient for vxn
////      b(equationIndex) = h.px
////      equationIndex += 1
////
////      a(equationIndex)(1) = 1 // Coefficient for y0n
////      a(equationIndex)(4) = -h.vy // Coefficient for vyn
////      b(equationIndex) = h.py
////      equationIndex += 1
////
////      a(equationIndex)(2) = 1 // Coefficient for z0n
////      a(equationIndex)(5) = -h.vz // Coefficient for vzn
////      b(equationIndex) = h.pz
////      equationIndex += 1
//      a(equationIndex) = Array(h.vy, -h.vx, -h.py, h.px, -1, 1)
//      b(equationIndex) = h.px * h.vy - h.py * h.vx
//      equationIndex += 1
//
//      a(equationIndex) = Array(h.vz, -h.vx, -h.pz, h.px, -1, 1)
//      b(equationIndex) = h.px * h.vz - h.pz * h.vx
//      equationIndex += 1
//    }
//
//    val solution = solveLinearSystem(a, b)
//
//    if (solution.length == 6) {
////      println(s"Solution found! Position: (${solution(0)}, ${solution(1)}, ${solution(2)}), " +
////        s"Velocity: (${solution(3)}, ${solution(4)}, ${solution(5)})"
////      )
//      val px = solution(0)
//      val vx = solution(2)
//
//      println(s"Solution found! Position: (${px}, Velocity: (${vx}})")
//    } else {
//      println("No unique solution found.")
//    }
//  }

  case class Hailstone(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long) {
    def intersectsInRangeIgnoreZ(other: Hailstone): Boolean = {
      def inRange(value: Double) = 200000000000000L <= value && value <= 400000000000000L

      // check intersection
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

  def forwardElimination(a: Array[Array[Double]], b: Array[Double]): Unit = {
    val numRows = a.length // The number of equations
    val numCols = a(0).length // The number of unknowns

    for (i <- 0 until numCols) {
      // Find the maximum element in the current column
      var maxEl = math.abs(a(i)(i))
      var maxRow = i
      for (k <- i + 1 until numRows) {
        if (math.abs(a(k)(i)) > maxEl) {
          maxEl = math.abs(a(k)(i))
          maxRow = k
        }
      }

      // Swap the maximum row with the current row
      val tempA = a(i)
      a(i) = a(maxRow)
      a(maxRow) = tempA
      val tempB = b(i)
      b(i) = b(maxRow)
      b(maxRow) = tempB

      // Make all rows below this one 0 in the current column
      for (k <- i + 1 until numRows) {
        val c = -a(k)(i) / a(i)(i)
        for (j <- i until numCols) {
          a(k)(j) += c * a(i)(j)
        }
        b(k) += c * b(i)
      }
    }
  }

  def backSubstitution(a: Array[Array[Double]], b: Array[Double]): Option[Array[Double]] = {
    val numRows = a.length    // The number of equations
    val numCols = a(0).length // The number of unknowns
    val x = new Array[Double](numCols)

    // Start back substitution if the last numCols rows form an upper triangular matrix.
    if (numRows >= numCols) {
      for (i <- numCols - 1 to 0 by -1) {
        // Check for zero diagonal element indicating no unique solution
        if (a(i)(i) == 0) return None

        x(i) = b(i) / a(i)(i)
        for (j <- 0 until i) {
          b(j) -= a(j)(i) * x(i)
        }
      }
      Some(x) // Return the solution if found
    } else {
      None // No solution if the system is underdetermined
    }
  }

  def solveLinearSystem(a: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    forwardElimination(a, b)
    backSubstitution(a, b).get
  }

  def invertMatrix(matrix: Array[Array[Double]]): Option[Array[Array[Double]]] = {
    val n = matrix.length
    val inverse = Array.tabulate(n)(i => Array.tabulate(n)(j => if (i == j) 1.0 else 0.0))

    // Implementing Gaussian Elimination
    for (i <- 0 until n) {
      // Find pivot
      var max = matrix(i)(i)
      var pivotRow = i
      for (row <- i + 1 until n) {
        if (math.abs(matrix(row)(i)) > max) {
          max = math.abs(matrix(row)(i))
          pivotRow = row
        }
      }

      // Check for singular matrix
      if (matrix(pivotRow)(i) == 0) return None

      // Swap rows in both matrices
      val temp = matrix(i)
      matrix(i) = matrix(pivotRow)
      matrix(pivotRow) = temp

      val tempId = inverse(i)
      inverse(i) = inverse(pivotRow)
      inverse(pivotRow) = tempId

      // Normalize the pivot row
      for (j <- 0 until n) {
        inverse(i)(j) /= max
      }
      for (j <- i + 1 until n) {
        matrix(i)(j) /= max
      }
      matrix(i)(i) = 1.0

      // Eliminate
      for (k <- 0 until n) {
        if (k != i) {
          val factor = matrix(k)(i)
          for (j <- 0 until n) {
            matrix(k)(j) -= factor * matrix(i)(j)
            inverse(k)(j) -= factor * inverse(i)(j)
          }
          matrix(k)(i) = 0
        }
      }
    }

    Some(inverse)
  }
}
