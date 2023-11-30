package utility

import scala.reflect.ClassTag

class Grid2D[T: ClassTag](val entries: Array[Array[T]]) {
  lazy val rowCount: Int = entries.length
  lazy val colCount: Int = if (rowCount > 0) entries.head.length else 0

  def at(row: Int, col: Int): T = entries(row)(col)
  def getRow(row: Int): Array[T] = entries(row)
  def getCol(col: Int): Array[T] = entries.map(_(col))

  lazy val entriesByColumn: Array[Array[T]] = entries.transpose
  
  def contains(row: Int, col: Int): Boolean = row >= 0 && col >= 0 && row < rowCount && col < colCount 

  override def toString: String = entries.map(_.mkString("[", ", ", "]")).mkString("\n")
}
