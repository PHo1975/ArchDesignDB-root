package definition.data

import java.awt.{BasicStroke, Color}

import util.StringUtils

import scala.collection.mutable

class LineStyle(val ix: Int, val name: String, val dots: Array[Float]) {
  def this(nix: Int, data: InstanceData) = this(nix, data.fieldValue.head.toString, data.fieldValue(1).toString.trim.split(';').map(StringUtils.stringToFloat))

  override def toString: String = name

  def equalDots(otherDots: Seq[Float]): Boolean =
    if (otherDots.size != dots.length) false
    else {
      for (i <- dots.indices)
        if (Math.abs(dots(i) - otherDots(i)) > StyleService.dotCompareTreshold) return false
      true
    }
}


object StyleService {

  final val dotCompareTreshold = 0.00001f
  final val alpha: Int = 170 << 24
  val alphaColorMap: mutable.HashMap[Int, Color] = collection.mutable.HashMap[Int, Color]()

  def getAlphaColor(col: Int): Color =
    if (alphaColorMap.contains(col)) alphaColorMap(col)
    else {
      val newCol = new Color(col + alpha, true)
      alphaColorMap(col) = newCol
      newCol
    }
}


trait AbstractLineStyleHandler {
  def createStroke(scale: Double, width: Float, styleIx: Int): BasicStroke
}

