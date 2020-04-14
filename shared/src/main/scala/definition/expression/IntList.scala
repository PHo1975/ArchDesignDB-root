package definition.expression
import java.io.{DataInput, DataOutput}

import definition.typ.DataType

case class IntList(list:Array[Int]) extends Constant {
  override def getType: DataType.Value = DataType.IntListTyp

  override def write(file: DataOutput): Unit = {
    file.writeByte(DataType.IntListTyp.id)
    file.writeInt(list.length)
    for(i<-list) file.writeInt(i)
  }

  override def encode: String = "$Z"+getTerm

  override def toInt: Int = list.length

  override def toLong: Long = toInt

  override def toDouble: Double = toInt.toDouble

  override def toBoolean: Boolean = list.nonEmpty

  override def getNative: Any = list

  override def toString=getTerm

  override def getTerm: String = list.mkString("[", "," ,"]")
}

object IntList {
  def decode(text:String): (IntList, Int) ={
    val end = text.indexOf(']', 3)
    val parts = text.substring(3, end).split(',')
    (new IntList(parts.map(_.toInt)),end+1)
  }

  def apply(in: DataInput):IntList={
    val num=in.readInt()
    IntList( (for(i <-0 until num) yield in.readInt()).toArray)
  }
}
