/**
  * Author: Peter Started:03.08.2010
  */
package definition.expression

import java.io.{DataInput, DataOutput}

import definition.data.Reference
import definition.typ.DataType

/** Expresses a reference to the value of another field in this or another object
  *
  * @param remType optional type of the remote object. can be ommittet when the remote object
  *        has the same type as this one
  * @param remInst optional instance of the remote object. can be ommitted when the referred field
  *        is in the same instance
  * @param remField data field of the remote object
  * @param cachedValue the current Value of the remote object field
  */
case class FieldReference(remType: Option[Int], remInst: Option[Int], remField: Byte, var cachedValue: Constant = EMPTY_EX) extends Expression {

  lazy val term: String = {
    "#" + (remType match {
      case Some(t) => "T" + t.toString
      case _ => ""
    }) +
      (remInst match {
        case Some(i) => "I" + i.toString
        case _ => ""
      }) + "F" + remField.toString
  }

  def getType: DataType.Value = DataType.FieldRefTyp

  def getValue: Constant = cachedValue

  override def createCopy(): Expression = new FieldReference(remType, remInst, remField, cachedValue)

  def getChildCount: Int = 0

  def getChildNr(ix: Int): Expression = null

  def getTerm: String = term

  override def getReadableTerm: String = cachedValue.getReadableTerm

  override def getReadableTerm(format:String): String = cachedValue.getReadableTerm(format)

  def getNative: String = toString

  override def toString: String = "Ref[" + remType + "," + remInst + "," + remField + (if (cachedValue.isNullConstant) "" else ",cv:" + cachedValue) + "]"

  // overrides equals to allowing compare 2 FieldReference objects with different cache values
  override def equals(other: Any): Boolean =
    other match {
      case that: FieldReference =>
        (that canEqual this) &&
          remType == that.remType &&
          remInst == that.remInst &&
          remField == that.remField
      case _ => false
    }

  override def hashCode: Int = 41 * (41 + remType.hashCode) + 1013 * (3 + remInst.hashCode) + remField.toInt

  def isConstant: Boolean = {false}

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.FieldRefTyp.id)
    remType match {
      case Some(t) => file.writeInt(t)
      case _ => file.writeInt(0)
    }
    remInst match {
      case Some(i) => file.writeInt(i)
      case _ => file.writeInt(0)
    }
    file.writeByte(remField)
    //System.out.println("write "+toString+" cached Value:"+cachedValue)
    cachedValue.write(file)
  }

  def setCachedValue(newVal: Constant): Unit = cachedValue = newVal

  /** returns a new FieldReference with qualified Type and Instance fields
    *
    * @param targetRef the reference of the target instance where this FieldReference is located
    * @return a qualified Reference with specified type and instance information
    */
  def qualifyWith(targetRef: Reference): FieldReference = {
    if (remInst.isDefined) {
      if (remType.isDefined) this
      else new FieldReference(Some(targetRef.typ), remInst, remField)
    }
    else new FieldReference(Some(targetRef.typ), Some(targetRef.instance), remField)
  }

  def encode: String = "$R(" + (remType match {case Some(t) => t.toString; case _ => ""}) + ";" +
    (remInst match {case Some(i) => i.toString; case _ => ""}) + ";" +
    remField.toString + ")"
}


object FieldReference {
  def decode(text: String): (FieldReference, Int) = {
    val end = text.indexOf(')')
    val parts = text.substring(3, end).split(';')
    val typ = if (parts(0).length == 0) None else Some(parts(0).toInt)
    val inst = if (parts(1).length == 0) None else Some(parts(1).toInt)
    (new FieldReference(typ, inst, parts(2).toByte), end + 1)

  }

  private[expression] def apply(file: DataInput): Expression = {
    val t = file.readInt
    val i = file.readInt
    val f = file.readByte
    val ret = new FieldReference(if (t == 0) None else Some(t), if (i == 0) None else Some(i), f, Expression.read(file).asInstanceOf[Constant])
    //System.out.println(" read "+ret+" cv: "+ret.cachedValue )
    ret
  }
}
