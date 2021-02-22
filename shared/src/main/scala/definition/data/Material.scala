package definition.data

import java.awt.BasicStroke

class Material(val inst:Int, val name:String, val hatch:Int, val priority:Int)  {
  def this(data:InstanceData) = this(data.ref.instance,data.fieldValue(1).toString,data.fieldValue(2).toInt,data.fieldValue(6).toInt)
  override def toString: String =name
}

trait AbstractMaterialHandler {
  def getMaterial(inst:Int):Option[Material]
}

object AbstractHandlers{
  implicit object MaterialHandler extends AbstractMaterialHandler{
    def getMaterial(inst:Int):Option[Material]= None
  }
  implicit object LineStyleHandler extends AbstractLineStyleHandler{
    override def createStroke(scale: Double, width: Float, styleIx: Int): BasicStroke = null
    override def styles: Seq[LineStyle] = Seq.empty
  }
}

case class ShellLayer(material:Material, thickness:Double, lineFrom:Int, hatchFrom:Int, lineThick:Int, lineStyle:LineStyle, role:Int,
                      priority:Int,color:Int)                      {
  def this(data:InstanceData) (implicit materialHandler:AbstractMaterialHandler, lineStyleHandler:AbstractLineStyleHandler) =
    this(materialHandler.getMaterial(data.fieldValue.head.toInt).getOrElse(throw new IllegalArgumentException("cant find Material "+data.fieldValue.head.toInt)),
    data.fieldValue(1).toDouble,data.fieldValue(2).toInt,data.fieldValue(3).toInt,
    data.fieldValue(4).toInt,lineStyleHandler.styles(data.fieldValue(5).toInt),data.fieldValue(6).toInt,
      data.fieldValue(7).toInt,data.fieldValue(8).toInt)
  def rolePriority: Int =4-role
  override def toString: String =(if(thickness==0)"" else "%3.2f".format(thickness*100)+" cm ") +material.name+"(P:"+priority+")"
}


case class Composition(ix:Int, name:String, typ:Int, shellLayers:Seq[ShellLayer])  {
  def this(data:InstanceData,layers:Seq[ShellLayer] )= {
    this(data.fieldValue.head.toInt,data.fieldValue(1).toString,data.fieldValue(2).toInt,layers)
  }
  override def toString: String =ix.toString+" "+name+", Schichten:\n  "+shellLayers.mkString("\n  ")
}