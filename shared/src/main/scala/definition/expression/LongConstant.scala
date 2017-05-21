/**
  * Author: Peter Started:05.09.2010
  */
package definition.expression

import java.io.DataOutput
import definition.typ.DataType

/**
  *
  */
case class LongConstant(n: Long) extends Constant {

  def getType: DataType.Value = DataType.LongTyp

  def getTerm: String = String.valueOf(n)

  def toInt: Int = n.toInt

  def toDouble: Double = n.toDouble

  def toLong: Long = n

  def toBoolean: Boolean = n > 0

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.LongTyp.id)
    file.writeLong(n)
  }

  def getNative: Long = n

  override def isNumberConstant = true

  def encode: String = "$L" + n

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)

  override def toString: String = String.valueOf(n)
}