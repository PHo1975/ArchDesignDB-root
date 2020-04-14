package building

import definition.data.{InstanceData, Reference}
import definition.expression.Plane3D

case class Plane(ref:Reference,name:String,plane:Plane3D) {
  def this(data:InstanceData)=this(data.ref,data.fieldValue(0).toString,new Plane3D(data.fieldValue(1).toVector,data.fieldValue(2).toVector))
}

case class Room(ref:Reference,name:String) {
  def this(data:InstanceData)=this(data.ref,data.fieldValue(0).toString)
}


