package util.clipping

object PathIterator {
  val WIND_EVEN_ODD = 0
  val WIND_NON_ZERO = 1
  val SEG_MOVETO = 0
  val SEG_LINETO = 1
  val SEG_QUADTO = 2
  val SEG_CUBICTO = 3
  val SEG_CLOSE = 4
}

trait PathIterator {
  def getWindingRule: Int

  def isDone: Boolean

  def next(): Unit

  def currentSegment(var1: Array[Float]): Int

  def currentSegment(var1: Array[scala.Double]): Int
}
