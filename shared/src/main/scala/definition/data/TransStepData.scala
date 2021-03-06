/**
  * Author: Peter Started:01.11.2010
  */
package definition.data

import java.io._


case class LogIndexSet(transTyp: TransType.Value, trID: Int, typ: Int, inst: Int, dataPos: Long, dataLength: Int) {
  override def toString: String = transTyp.toString + " [" + typ + "," + inst + "] dataPos:" + dataPos + " length:" + dataLength
}


case class TransStepData(trID: Int, time: Int, userID: String, firstInst: InstanceData, multiInst: Boolean, action: String,
                         createType: Int) {
  //System.out.println("transstep "+trID)
  def write(out: DataOutput): Unit = {
    out.writeInt(trID)
    out.writeInt(time)
    out.writeUTF(userID)
    if (firstInst == null) Reference(0, 0).write(out)
    else {
      firstInst.ref.write(out)
      firstInst.write(out)
    }

    out.writeBoolean(multiInst)
    out.writeUTF(action)
    out.writeInt(createType)
  }
}


object TransStepData {
  def read(in: DataInput): TransStepData = {
    new TransStepData(in.readInt, in.readInt, in.readUTF, InstanceData.read(Reference(in.readInt, in.readInt), in, nhasChildren = true),
      in.readBoolean, in.readUTF, in.readInt)
  }
}


/**
  * defines the transaction types
  */
object TransType extends Enumeration {
  type TransType = Value
  val created = Value("created")
  val dataChanged = Value("Data changed")
  val propertyChanged = Value("Prop changed")
  val linksChanged = Value("Links changed")
  val collFuncChanged = Value("CollFunc changed")
  val deleted = Value("deleted")
  val undefined= Value("undefined")
}
