/**
  * Author: Peter Started:12.08.2010
  */
package definition.data

import java.io.DataOutput

import definition.expression.{CollectingFuncCall, Constant}


/** a Flag to signal that a field in a parent instance has a collectingFunction
  *
  */
sealed abstract class CollFuncResult(val funcName: String, val childType: Int, val childField: Byte, val parentField: Byte,
                                     val parentPropField: Byte) {

  def write(file: DataOutput): Unit = {
    file.writeUTF(funcName)
    file.writeInt(childType)
    file.writeByte(childField)
    file.writeByte(parentField)
    file.writeByte(parentPropField)
  }

  /** checks if this result fits to the given function call
    *
    * @param call     the function call to test
    * @param pfieldNr the field nr where  the call is located
    * @return true if they fit
    */
  def fitsToFuncCall(call: CollectingFuncCall, pfieldNr: Byte): Boolean = {
    pfieldNr == parentField && call.name == funcName && call.childType == childType && call.childField == childField &&
      call.propertyField == parentPropField
  }
}


class SingleCollFuncResult(override val funcName: String, override val childType: Int, override val childField: Byte, override val parentField: Byte,
                           override val parentPropField: Byte, val resultValue: Constant) extends
  CollFuncResult(funcName, childType, childField, parentField, parentPropField) {
  override def write(file: DataOutput): Unit = {
    file.writeBoolean(false)
    super.write(file)
    resultValue.write(file)
  }

  override def toString: String = " SingleResult " + funcName + " childType:" + childType + " childField:" + childField + " parField:" + parentField + " parPropField:" +
    parentPropField + " result:" + resultValue
}


class ListCollFuncResult(override val funcName: String, override val childType: Int, override val childField: Byte, override val parentField: Byte,
                         override val parentPropField: Byte, val resultList: List[(Reference, Constant)]) extends
  CollFuncResult(funcName, childType, childField, parentField, parentPropField) {
  override def write(file: DataOutput): Unit = {
    file.writeBoolean(true)
    super.write(file)
    file.writeInt(resultList.length)
    for (r <- resultList) {
      r._1.write(file)
      r._2.write(file)
    }
  }

  override def toString: String = " ListResult " + funcName + " childType:" + childType + " childField:" + childField + " parField:" + parentField + " parPropField:" +
    parentPropField + " resultList:" + resultList
}

//*********************************************************************************************************************


