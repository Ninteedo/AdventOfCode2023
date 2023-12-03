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

  override def toString: String = entries.map(_.mkString("[", ", ", "]")).mkString("\n")
}
