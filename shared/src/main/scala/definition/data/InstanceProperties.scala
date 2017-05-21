/**
  * Author: Peter Started:28.07.2010
  */
package definition.data

import java.io.{ DataInput, DataOutput }
import definition.typ.AllClasses
import scala.collection.immutable.IndexedSeq


/** Stores the Property Information of an instance
  * propertyFields: Array of the propertyFieldsData of an instance
  */
class InstanceProperties(override val ref: Reference, val propertyFields: Array[PropertyFieldData])
  extends Referencable {

  //TODO: convert to indexseq

  override def write(file: DataOutput): Unit = {

    file.writeByte(propertyFields.length)
    for (p <- propertyFields)
      p.write(file)
  }

  def dataLength: Int = {
    var result = 1
    for (p <- propertyFields) result += p.dataLength
    result
  }

  def addChildInstance(field: Byte, newInst: Reference, atPos: Int): InstanceProperties =
    changeField(field, getFieldOrCreate(field).addPropertyInstance(newInst, atPos))

  def addChildInstances(field: Byte, list: Iterable[Reference]): InstanceProperties =
    changeField(field, getFieldOrCreate(field).addPropertyInstances(list))

  def changeField(field: Byte, newValue: PropertyFieldData): InstanceProperties = {
    val newFields = if (field >= propertyFields.length) {
      util.Log.w("change propField ref " + ref + " access to field " + field + " over existing size " + propertyFields.length)
      val narray = new Array[PropertyFieldData](field + 1)
      for (i <- propertyFields.indices)
        narray(i) = propertyFields(i)
      narray
    } else propertyFields.clone()
    newFields(field) = newValue
    new InstanceProperties(ref, newFields)
  }

  def getFieldOrCreate(fieldIx: Int): PropertyFieldData =
    if (fieldIx < propertyFields.length) propertyFields(fieldIx)
    else {
      val pfields = AllClasses.get.getClassByID(ref.typ).propFields
      if (fieldIx < pfields.length) new PropertyFieldData(pfields(fieldIx).single, IndexedSeq.empty)
      else throw new IllegalArgumentException("add child getPropertyField " + fieldIx + " index is out of bounds for " + ref + " propfield Size " + pfields.length)
    }

  def removeChildInstance(field: Byte, remInst: Reference): InstanceProperties =
    changeField(field, propertyFields(field).removePropertyInstance(remInst))

  def moveChildInstanceToPos(field: Byte, inst: Reference, pos: Int): InstanceProperties =
    changeField(field, propertyFields(field).moveInstanceToPos(inst, pos))

  def sortChildrenByField(field: Byte, instField: Int, retriever: DataRetriever): InstanceProperties =
    changeField(field, propertyFields(field).sortChildren(instField, retriever))

  def hasChildren: Boolean = propertyFields.exists(_.propertyList.nonEmpty)

  override def toString: String = "InstProp" + ref.toString + " " + propertyFields.mkString(",")
}


object InstanceProperties {

  def read(ninstRef: Reference, file: DataInput): InstanceProperties = {
    val count: Int = file.readByte
    val propArray = new Array[PropertyFieldData](count.toInt)
    for (i <- 0 until count)
      propArray(i) = PropertyFieldData(file)
    new InstanceProperties(ninstRef, propArray)
  }

}