/**
  * Author: Peter Started:24.04.2011
  */
package definition.expression


import java.io._

import definition.typ.DataType

/**
  *
  */
case class BlobConstant(data: Array[Byte]) extends Constant {

  def getType: DataType.Value = {DataType.BlobTyp}

  /*def createCopy(): Expression = {
  	val nArray=new Array[Byte](data.length)
  	java.lang.System.arraycopy(data, 0, nArray, 0, data.length)
  	new BlobConstant(nArray)
  }*/

  def getTerm: String = toString

  override def toString: String = "[Blob size=" + data.length + "]"

  def toInt = 0

  def toDouble = 0d

  def toLong = 0L

  def toBoolean = false

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.BlobTyp.id)
    file.writeInt(data.length)
    file.write(data)
  }

  def getNative: Array[Byte] = data

  def read[T](func: DataInput => T): T = {
    val byteStream = new DataInputStream(new ByteArrayInputStream(data))
    func(byteStream)
  }

  override def isNumberConstant = false

  def fillData(func: DataOutput => Unit): BlobConstant = {
    val byteStream = new ByteArrayOutputStream()
    val outStream = new DataOutputStream(byteStream)
    func(outStream)
    new BlobConstant(byteStream.toByteArray)
  }

  def encode = "$X"
}


object BlobConstant {
  def fillData(func: DataOutput => Unit): BlobConstant = {
    val byteStream = new ByteArrayOutputStream()
    val outStream = new DataOutputStream(byteStream)
    func(outStream)
    new BlobConstant(byteStream.toByteArray)
  }

  private[expression] def apply(file: DataInput) = {
    val length = file.readInt
    val newArray = new Array[Byte](length)
    file.readFully(newArray)
    new BlobConstant(newArray)
  }
}