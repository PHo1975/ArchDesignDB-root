/**
  * Author: Peter Started:18.07.2010
  */
package definition.expression

import java.io.DataOutput

import definition.typ.DataType
import util.StringUtils

import scala.util.control.NonFatal

/**
  * String Constant type
  */
case class StringConstant(n: String) extends Constant {

  def toInt: Int = try {n.toInt} catch {case NonFatal(_) => 0}

  def toLong: Long = try {n.toLong} catch {case NonFatal(_) => 0L}

  def toDouble: Double = try {n.toDouble} catch {case NonFatal(_) => 0}

  def toBoolean: Boolean = n.trim match {
    case "1" | "true" | "TRUE" | "True" => true
    case _ => false
  }

  def getType: DataType.Value = DataType.StringTyp

  //def createCopy(): Expression = { StringConstant(n) }

  def getTerm: String = '"' + n + '"'

  override def toString: String = n

  override def equals(other: Any): Boolean = other match {
    case that: StringConstant => n == that.n
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[StringConstant]

  override def hashCode: Int = n.hashCode

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.StringTyp.id)
    //System.out.println("StringID:" +DataType.StringTyp.id)
    //Thread.dumpStack()
    file.writeUTF(n)
  }

  def getNative: String = n

  def encode: String = StringUtils.encode(n)

  override def containsString(st: String, checkNumbers: Boolean): Boolean = n.toLowerCase.contains(st.toLowerCase)
}