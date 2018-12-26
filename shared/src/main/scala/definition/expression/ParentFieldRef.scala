/**
  * Author: Peter Started:10.04.2011
  */
package definition.expression

import java.io.{DataInput, DataOutput}

import definition.typ.DataType

/**
  *
  */
case class ParentFieldRef(ownerIx: Byte, fieldNr: Byte, var cachedValue: Constant = EMPTY_EX) extends Expression {

  def getType: DataType.Value = DataType.ParentRefTyp

  def getValue: Constant = cachedValue

  def setValue(newValue: Constant): Unit = cachedValue = newValue

  override def createCopy(): Expression = new ParentFieldRef(ownerIx, fieldNr, cachedValue)

  def getChildCount: Int = 0

  def getChildNr(ix: Int): Expression = null

  def isConstant: Boolean = false

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.ParentRefTyp.id)
    file.writeByte(ownerIx)
    file.writeByte(fieldNr)
    cachedValue.write(file)
  }

  override def toString: String =
    getTerm + (if (cachedValue.isNullConstant) "" else " cv:" + cachedValue)


  def getTerm: String = "#P" + ownerIx + "F" + fieldNr

  override def getReadableTerm: String = cachedValue.getReadableTerm

  override def getReadableTerm(format:String): String = cachedValue.getReadableTerm(format)

  override def equals(other: Any): Boolean =
    other match {
      case that: ParentFieldRef =>
        (that canEqual this) &&
          ownerIx == that.ownerIx &&
          fieldNr == that.fieldNr
      case _ => false
    }

  override def hashCode: Int = 41 * (41 + ownerIx.hashCode) + fieldNr.toInt + 1

  def encode: String = "$P" + ownerIx + ";" + fieldNr

}


object ParentFieldRef {
  def decode(text: String): (ParentFieldRef, Int) = {
    val sepPos = text.indexOf(';', 2)
    val ow = text.substring(2, sepPos).toByte
    val instEnd = CurrencyConstant.findEnd(text, sepPos + 1)
    val field = text.substring(sepPos + 1, instEnd).toByte
    (ParentFieldRef(ow, field), instEnd)
  }

  private[expression] def apply(file: DataInput): Expression =
    new ParentFieldRef(file.readByte, file.readByte, Expression.readConstant(file))
}