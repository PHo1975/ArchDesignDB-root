/**
  * Author: Peter Started:28.05.2011
  */
package definition.expression

import java.io.{ DataInput, DataOutput }
import definition.data.Reference
import definition.typ.DataType

/**
  *
  */
case class ObjectReference(typ: Int, instance: Int) extends Constant {
  def this(ref: Reference) = this(ref.typ, ref.instance)

  override def toObjectReference = new Reference(typ, instance)

  def toLong: Long = instance

  def toDouble: Double = toInt.toDouble

  def toBoolean: Boolean = toInt > 0

  def toInt: Int = typ

  def getNative: Any = this

  def getType = DataType.ObjectRefTyp

  //def createCopy=new ObjectReference(typ,instance)
  def getTerm: String = "(" + typ + "," + instance + ")"

  override def write(file: DataOutput): Unit = {
    file.writeByte(DataType.ObjectRefTyp.id)
    file.writeInt(typ)
    file.writeInt(instance)
  }

  def encode: String = "$O" + typ + ";" + instance
}


object ObjectReference {
  def apply(ref: Reference) = new ObjectReference(ref.typ, ref.instance)

  def decode(text: String): (ObjectReference, Int) = {
    val sepPos = text.indexOf(';', 2)
    val typ = text.substring(2, sepPos).toInt
    val instEnd = CurrencyConstant.findEnd(text, sepPos + 1)
    val inst = text.substring(sepPos + 1, instEnd).toInt
    (ObjectReference(typ, inst), instEnd)
  }

  private[expression] def apply(file: DataInput) =
    new ObjectReference(file.readInt, file.readInt)
}