package building

import definition.data.{InstanceData, Reference}
import definition.expression.IntList

case class Cell(ref:Reference,topPlane:Plane,bottomPlane:Plane,wallPlanes:Array[Plane],room:Option[Room]) {
  def this(data: InstanceData, model: AbstractBuildingModel) = this(data.ref, model.getPlane(data.fieldValue(0).toInt), model.getPlane(data.fieldValue(1).toInt),
    data.fieldValue(2) match {
      case il: IntList => for (el <- il.list) yield model.getPlane(el)
      case o => throw new IllegalArgumentException("no IntList in Cell " + o)
    }, model.getRoom(data.fieldValue(3).toInt))

}