package utility

import scala.reflect.ClassTag

class Grid2D[T: ClassTag](val entries: Array[Array[T]]) {
  lazy val rowCount: Int = entries.length
  lazy val colCount: Int = if (rowCount > 0) entries.head.length else 0

  def at(row: Int, col: Int): T = entries(row)(col)
  def at(pos: Point2D): T = at(pos.y, pos.x)
  def getRow(row: Int): Array[T] = entries(row)
  def getCol(col: Int): Array[T] = entries.map(_(col))

  lazy val entriesByColumn: Array[Array[T]] = entries.transpose

  def contains(row: Int, col: Int): Boolean = row >= 0 && col >= 0 && row < rowCount && col < colCount
  def contains(pos: Point2D): Boolean = contains(pos.y, pos.x)

  def adjacent(row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    contains(row1, col1) && contains(row2, col2) && (row1 != row2 || col1 != col2) && (row1 - row2).abs <= 1 && (col1 - col2).abs <= 1
  def adjacent(pos1: Point2D, pos2: Point2D): Boolean = adjacent(pos1.y, pos1.x, pos2.y, pos2.x)

  def indexWhere(f: T => Boolean): Option[Point2D] = {
    entries.zipWithIndex.find { case (row, rowNum) => row.indexWhere(f) >= 0 } match {
      case Some((row, rowNum)) => Some(Point2D(row.indexWhere(f), rowNum))
      case None => None
    }
  }

  def indicesWhere(f: T => Boolean): List[Point2D] = {
    entries.zipWithIndex.flatMap {
      case (row, rowNum) => row.indices.filter(colNum => f(at(rowNum, colNum))).map(colNum => Point2D(colNum, rowNum))
    }.toList
  }

  def transpose: Grid2D[T] = new Grid2D(entriesByColumn)

  def updated(pos: Point2D, value: T): Grid2D[T] = {
    val newEntries = entries.clone()
    newEntries(pos.y) = newEntries(pos.y).clone()
    newEntries(pos.y)(pos.x) = value
    new Grid2D(newEntries)
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Grid2D[T] => entries.zip(other.entries).forall((a, b) => a sameElements b)
    case _ => false
  }

  lazy val hashCodeCache: Int = entries.hashCode()

  override def hashCode(): Int = hashCodeCache

  override def toString: String = entries.map(_.mkString("[", ", ", "]")).mkString("\n")

  def toGridString(f: T => String): String = entries.map(_.map(f).mkString("")).mkString("\n")
}

object Grid2D {
  def from2DCharArray[T: ClassTag](input: String, f: Char => T): Grid2D[T] = {
    val lines = input.split("\n")
    val entries = lines.map(_.strip.map(f).toArray)
    new Grid2D(entries)
  }
}
