package definition.data

import java.io.DataOutput
import java.io.DataInput

case class HolidayDefinition(name:String,month_easter:Int,day:Int,official:Boolean) {
  def this(data:InstanceData)=this(data.fieldValue.head.toString,data.fieldValue(1).toInt,
      data.fieldValue(2).toInt,data.fieldValue(3).toInt==1)
  def this(in:DataInput)= this(in.readUTF,in.readInt,in.readInt,in.readBoolean) 
  
  def write(out:DataOutput)= {
     out.writeUTF(name)
     out.writeInt(month_easter)
     out.writeInt(day)
     out.writeBoolean(official)
  }
}
   
