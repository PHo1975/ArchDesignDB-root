/**
  * Author: Peter Started:18.07.2010
  */
package definition.expression

import java.io.DataOutput

import definition.typ.DataType
import util.Log

import scala.util.control.NonFatal

/**
  *
  */
case class IntConstant(n: Int) extends Constant {

  def getType: DataType.Value = DataType.IntTyp

  def getTerm: String = String.valueOf(n)

  override def getReadableTerm(format:String):String= try {
    format.format(n)
  } catch {
    case NonFatal(e)=> Log.e("format:"+format+" value:"+n+" error:"+e);getTerm
  }

  def toInt: Int = n


  def toDouble: Double = n.toDouble

  def toLong: Long = n.toLong

  def toBoolean: Boolean = n > 0

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.IntTyp.id)
    file.writeInt(n)
  }

  def getNative: Int = n

  override def isNumberConstant = true

  def encode: String = "$I" + n

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)

  override def toString: String = String.valueOf(n)
}