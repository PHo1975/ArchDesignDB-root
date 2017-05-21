package definition.typ

import java.io.DataInput
import util.StoreClassInfo
import scala.xml.Elem

/**
  * Author: Peter Started:26.06.2010
  */

/**
  * Description of a single Datafield in a DataObject
  */
sealed trait AbstractFieldDefinition extends StoreClassInfo {
  def name: String

  def typ: DataType.Value

  def toXML: scala.xml.Elem

  def setName(newName: String): AbstractFieldDefinition

  def setType(newType: DataType.Value): AbstractFieldDefinition =
    if (newType == DataType.EnumTyp) EnumFieldDefinition(name, DataType.EnumTyp, 0)
    else new FieldDefinition(name, newType)

  def setEnumID(newID: Int) = EnumFieldDefinition(name, DataType.EnumTyp, newID)
}


case class FieldDefinition(name: String, typ: DataType.Value) extends AbstractFieldDefinition {
  override def toString: String = "Field[" + name + ":" + typ + "]"

  def toXML: Elem = <FD name={name} t={typ.id.toString}/>

  def classID: Int = 1

  def setName(newName: String) = new FieldDefinition(newName, typ)

}


case class EnumFieldDefinition(name: String, typ: DataType.Value = DataType.EnumTyp, enumID: Int) extends AbstractFieldDefinition {
  def toXML: Elem = <FD name={name} t={typ.id.toString} enumID={enumID.toString}/>

  def setName(newName: String) = EnumFieldDefinition(newName, DataType.EnumTyp, enumID)

  override def toString: String = "E" + super.toString + " eid:" + enumID

  def classID: Int = 2
}


object FieldDefinition {
  def fromXML(node: scala.xml.Node): AbstractFieldDefinition = {
    val name = (node \ "@name").text
    val typ = (node \ "@t").text.toInt
    if (typ == DataType.EnumTyp.id) EnumFieldDefinition(name, DataType.EnumTyp, (node \ "@enumID").text.toInt)
    else new FieldDefinition(name, DataType(typ))
  }

  def fromStream(in: DataInput): AbstractFieldDefinition = {
    val name = in.readUTF()
    val typ = in.readInt()
    if (typ == DataType.EnumTyp.id) EnumFieldDefinition(name, DataType.EnumTyp, in.readInt())
    else new FieldDefinition(name, DataType(typ))
  }
}