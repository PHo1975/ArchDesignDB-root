/**
  * Author: Peter Started:18.12.2010
  */
package definition.typ

import java.io.{DataInput, DataOutput}

import definition.data.{DataRetriever, InstanceData}

import scala.collection.JavaConverters._

/**
  *
  */
class EnumData(val name: String, val id: Int) {
  override def toString: String = name
}


class EnumDefinition(val name: String, val id: Int, val enumValues: collection.Map[String, Int]) {
  lazy val getElem: Map[Int, (String, Int)] = enumValues.map(a => a._2 -> a).toMap
  lazy val javaVect = new java.util.Vector[EnumData](enumValues.map(a => new EnumData(a._1, a._2)).asJavaCollection)

  override def toString: String = name + "(" + id + ")"

  def write(out: DataOutput): Unit = {
    out.writeUTF(name)
    out.writeInt(id)
    out.writeInt(enumValues.size)
    for (e <- enumValues) {
      out.writeUTF(e._1)
      out.writeInt(e._2)
    }
  }
}


object EnumDefinition {

  def apply(data: InstanceData, ret: DataRetriever): EnumDefinition = {
    val name = data.fieldValue(1).toString
    val id = data.fieldValue.head.toInt
    val enumValues = collection.mutable.LinkedHashMap[String, Int]()
    ret.getInstanceProperties(data.ref) match {
      case Some(propData) =>
        for (pField <- propData.propertyFields(0).propertyList) {
          val dat = ret.getInstanceData(pField)
          enumValues(dat.fieldValue(1).toString) = dat.fieldValue.head.toInt
        }

      case None => util.Log.e("EnumDef " + name + " is empty !")
    }
    new EnumDefinition(name, id, enumValues)
  }

  def apply(in: DataInput): EnumDefinition = {
    new EnumDefinition(in.readUTF, in.readInt, {
      val enumValues = collection.mutable.LinkedHashMap[String, Int]()
      for (_ <- 0 until in.readInt) enumValues(in.readUTF) = in.readInt
      enumValues
    })
  }

}


object NOENUM extends EnumDefinition("Undefined", 0, Map.empty) {
  override def toString = ""
}