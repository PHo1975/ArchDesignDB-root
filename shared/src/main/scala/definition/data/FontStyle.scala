/**
  * Author: Peter Started:29.12.2010
  */
package definition.data

import java.awt.font.{ FontRenderContext, LineMetrics }
import java.awt.geom.Rectangle2D
import java.awt.{ Color, Font }
import java.io.{ DataInput, DataOutput }
import definition.typ.SystemSettings
import scala.xml.Elem

/**
  *
  */
case class FontStyle(styleName: String, fontName: String, height: Float, bold: Boolean, italic: Boolean, underline: Boolean, color: Color) {

  lazy val font = new Font(fontName, javaStyle, (height * 72.0 / 25.4).toInt)
  lazy val lineMetrics: LineMetrics = font.getLineMetrics("QqpL", FontStyle.fontRenderCtx)
  lazy val mmHeight: Float = lineMetrics.getHeight * 25.4f / 72.0f * (if (underline) 1.1f else 1f)

  def javaStyle: Int = (if (bold) Font.BOLD else 0) + (if (italic) Font.ITALIC else 0)

  def graphStyle: Int = (if (bold) FontStyle.boldStyle else 0) + (if (italic) FontStyle.italicStyle else 0) + (if (underline) FontStyle.underlineStyle else 0)

  def write(out: DataOutput): Unit = {
    out.writeUTF(styleName)
    out.writeUTF(fontName)
    out.writeFloat(height)
    out.writeBoolean(bold)
    out.writeBoolean(italic)
    out.writeBoolean(underline)
  }

  def changeSize(scale: Float): FontStyle = new FontStyle(styleName, fontName, height * scale, bold, italic, underline, color)

  def toXML: Elem =
      <Font sname={styleName} fname={fontName} height={height.toString} bold={if (bold) "1" else "0"} italic={if (italic) "1" else "0"}
            uline={if (underline) "1" else "0"} color={color.getRGB.toString}/>


  def getStringBounds(st: String): Rectangle2D = font.getStringBounds(st, FontStyle.fontRenderCtx)

}


object FontStyle {
  final val boldStyle = 128
  final val italicStyle = 256
  final val underlineStyle = 512

  val fontRenderCtx: FontRenderContext = new FontRenderContext(null, true, true)
  val fontType: Int = SystemSettings().systemTypes("StampFont")

  def apply(in: DataInput): FontStyle =
    new FontStyle(in.readUTF, in.readUTF, in.readFloat, in.readBoolean, in.readBoolean, in.readBoolean, new Color(in.readInt))

  def fromXML(node: scala.xml.Node): FontStyle =
    new FontStyle((node \ "@sname").text, (node \ "@fname").text, (node \ "@height").text.toFloat, (node \ "@bold").text == "1",
      (node \ "@italic").text == "1", (node \ "@uline").text == "1", new Color((node \ "@color").text.toInt))

  def apply(data: InstanceData): FontStyle =
    new FontStyle(data.fieldValue.head.toString, data.fieldValue(1).toString, data.fieldValue(2).toDouble.toFloat,
      data.fieldValue(4).toBoolean, data.fieldValue(3).toBoolean, data.fieldValue(5).toBoolean, new Color(data.fieldValue(6).toInt))

}


class FontStyleList(val list: Seq[FontStyle]) {
  lazy val standardStyle: FontStyle = list.find(_.styleName.equalsIgnoreCase("Standard")) match {
    case Some(style) => /*println("Standard style found:"+style);*/ style
    case None => new FontStyle("Standard", "Arial", 10.0f, false, false, false, Color.black)
  }
  private val fontMap = list.map(a => a.styleName.toLowerCase -> a).toMap

  def write(out: DataOutput): Unit = {
    out.writeInt(list.size)
    list.foreach(_.write(out))
  }

  def toXML: Seq[Elem] = list.map(_.toXML)

  def getStyle(stName: String): FontStyle = if (fontMap.contains(stName.toLowerCase)) fontMap(stName.toLowerCase)
                                            else standardStyle
}


object FontStyleList {

  def apply(in: DataInput): FontStyleList =
    new FontStyleList(for (_ <- 0 until in.readInt) yield FontStyle(in))


  def fromXML(node: scala.xml.Node): FontStyleList =
    new FontStyleList((node \\ "Font").map(FontStyle.fromXML))

}