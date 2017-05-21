/**
  * Author: Peter Started:18.07.2010
  */
package definition.expression

import java.io.DataOutput
import definition.typ.DataType

/**
  *
  */
case class DoubleConstant(n: Double) extends Constant {

  def getType: DataType.Value = DataType.DoubleTyp

  def getTerm: String = toString

  override def toString: String = String.valueOf(n)

  def toInt: Int = n.round.toInt

  def toLong: Long = n.round

  def toDouble: Double = n

  def toBoolean: Boolean = n > 0

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.DoubleTyp.id)
    file.writeDouble(n)
  }

  def getNative: Double = n

  override def isNumberConstant = true

  def encode: String = "$G" + n.toString

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)
}


object DoubleConstant {
  val allowedChars: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-')

  //val format=new java.text.DecimalFormat("0.0########")
  def decode(text: String): (DoubleConstant, Int) = {
    val end = findEnd(text, 2)
    (new DoubleConstant(text.substring(2, end).toDouble), end)
  }

  def findEnd(text: String, startPos: Int): Int = {
    var pos = startPos
    while (pos < text.length && allowedChars.contains(text.charAt(pos)))
      pos += 1
    pos
  }
}


