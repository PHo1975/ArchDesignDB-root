/**
  * Author: Peter Started:19.12.2010
  */
package definition.data

import java.awt.Graphics2D
import definition.expression.{ Constant, Expression }
import definition.typ.SystemSettings
import scala.xml.Elem


/** description of a PrintForm
  *
  */
case class FormDescription(name: String, inst: Int, isIteratorForm: Boolean, isLandscape: Boolean,
                           left: Int, right: Int, top: Int, bottom: Int, params: Seq[ParameterDescription], fonts: FontStyleList) {
  //println("create Form "+name+" inst:"+inst)		
  def toXML: Elem = {
    <Form name={name} inst={inst.toString} iterForm={if (isIteratorForm) "1" else "0"} isLandScape={if (isLandscape) "1" else "0"} l={left.toString}
          r={right.toString} t={top.toString} b={bottom.toString}>
      {for (p <- params) yield p.toXML}{fonts.toXML}
    </Form>
  }

  def updateFontMetrics(g: Graphics2D): Unit = {
    //FontStyle.graphics=g
    //fonts.list.foreach(_.updateFontMetrics)
  }

  override def toString: String = name + "  (" + (if (isLandscape) "Querformat)" else "HochFormat)")

}


object FormDescription {
  def toMM(f: Double): Float = (f * 25.4 / 72.0).toFloat

  def fromMM(mm: Float): Float = (mm * 72.0 / 25.4).toFloat

  def apply(data: InstanceData, params: Seq[ParameterDescription], fontChildData: Seq[InstanceData]): FormDescription = {

    val fontList = new FontStyleList(for (c <- fontChildData; if c.ref.typ == FontStyle.fontType)
      yield FontStyle(c))
    new FormDescription(data.fieldValue.head.toString, data.ref.instance, data.fieldValue(1).toInt == 0, data.fieldValue(2).toInt == 1,
      data.fieldValue(3).toInt, data.fieldValue(4).toInt, data.fieldValue(5).toInt, data.fieldValue(6).toInt,
      params, fontList)
  }

  def fromXML(node: scala.xml.Node): FormDescription = {
    new FormDescription((node \ "@name").text, (node \ "@inst").text.toInt, (node \ "@iterForm").text == "1", (node \ "isLandScape").text == "1",
      (node \ "@l").text.toInt, (node \ "@r").text.toInt, (node \ "@t").text.toInt, (node \ "@b").text.toInt,
      for (d <- node \\ "Param") yield ParameterDescription.fromXML(d), FontStyleList.fromXML(node))
  }
}


case class ParameterDescription(name: String, dataType: Int, desc: String, defaultValue: Constant) {
  def toXML: Elem = {
      <Param name={name} type={dataType.toString} desc={desc} def={defaultValue.encode}/>
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: ParameterDescription =>
        (that canEqual this) && name == that.name && dataType == that.dataType
      case _ => false
    }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParameterDescription]

  override def hashCode: Int = name.hashCode + dataType.hashCode * 41
}


object ParameterDescription {
  val parameterType: Int = SystemSettings().systemTypes("PrintFormParameter")

  def apply(data: InstanceData) =
    new ParameterDescription(data.fieldValue.head.toString, data.fieldValue(1).toInt, data.fieldValue(2).toString, data.fieldValue(3))

  def fromXML(node: scala.xml.Node) = new ParameterDescription((node \ "@name").text, (node \ "@type").text.toInt, (node \ "@desc").text,
    Expression.decode((node \ "@def").text)._1.getValue)
}