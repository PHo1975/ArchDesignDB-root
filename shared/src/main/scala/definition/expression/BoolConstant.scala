/**
  * Author: Peter Started:25.09.2010
  */
package definition.expression

import java.io.DataOutput

import definition.typ.DataType

/**
  *
  */
case class BoolConstant(v: Boolean) extends Constant {

  def toInt: Int = {if (v) 1 else 0}

  def toLong: Long = {if (v) 1L else 0L}

  def toDouble: Double = {if (v) 1d else 0d}

  def toBoolean: Boolean = v

  def getType: DataType.Value = {DataType.BoolTyp}

  //def createCopy(): Expression = { new BoolConstant(v) }

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.BoolTyp.id)
    file.writeBoolean(v)
  }

  def getNative: Boolean = v

  override def toString: String = getTerm

  def getTerm: String = {if (v) "true" else "false"}

  def encode: String = "$T" + (if (v) "1" else "0")
}