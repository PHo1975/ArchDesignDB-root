/**
  * Author: Peter Started:27.12.2010
  */
package definition.data

import definition.expression._
import util.{Log, StringUtils}

import java.awt.font.TextLayout
import java.awt.geom._
import java.awt.{BasicStroke, Color, Font, Graphics2D}
import java.io.{DataInput, DataOutput, File}
import javax.imageio.ImageIO
import scala.util.control.NonFatal

/**
  *
  */
abstract class PrintElement(val bounds: Rectangle2D.Float) {

  def write(out: DataOutput): Unit = {
    out.writeByte(getElementType.id.toByte)
    out.writeFloat(bounds.x)
    out.writeFloat(bounds.y)
    out.writeFloat(bounds.width)
    out.writeFloat(bounds.height)
  }

  def getElementType: PrintElType.Value

  def print(g: Graphics2D, ctx: RenderContext):Unit
}


abstract class NoBoundsPrintElement(val getElementType: PrintElType.Value) extends PrintElement(null) {
  override def write(out: DataOutput): Unit = {
    out.writeByte(getElementType.id.toByte)
  }
}


object PageBreakMarker extends NoBoundsPrintElement(PrintElType.PageBreak) {
  def print(g: Graphics2D, ctx: RenderContext): Unit = {}
}


class ClipPrintElement(nbounds: Rectangle2D.Float) extends PrintElement(nbounds) {
  def getElementType:PrintElType.Value = PrintElType.ClipPrintElement

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    ctx.oldClip = Option(g.getClip)
    g.clip(new Rectangle2D.Float(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.width), ctx.toUnit(bounds.height)))
    //println("clip old:"+ctx.oldClip.get+"\nnew:"+g.getClip)
  }
}


class RestoreClipPrintElement() extends NoBoundsPrintElement(PrintElType.ClipRestoreElement) {
  def print(g: Graphics2D, ctx: RenderContext): Unit = ctx.oldClip match {
    case Some(clip)=> g.setClip(clip); //println("restore Clip "+clip)
    case None => g.setClip(null); println("Restore Clip no old Clip")
  }
}


object DecoreMarkerElement extends NoBoundsPrintElement(PrintElType.DecoreMarker) {
  def print(g: Graphics2D, ctx: RenderContext): Unit = {}
}


object PrintElType extends Enumeration {
  val TextField: PrintElType.Value = Value("TextElement")
  val Line: PrintElType.Value = Value("LineElement")
  val TextArea: PrintElType.Value = Value("TextArea")
  val Rect: PrintElType.Value = Value("RectElement")
  val Arc: PrintElType.Value = Value("ArcElement")
  val Poly: PrintElType.Value = Value("PolyElement")
  val Ellipse: PrintElType.Value = Value("Ellipse")
  val GraphText: PrintElType.Value = Value("GraphText")
  val Filler: PrintElType.Value = Value("Filler")
  val DimLine: PrintElType.Value = Value("DimLineElement")
  val PageBreak: PrintElType.Value = Value("PageBreak")
  val ClipPrintElement: PrintElType.Value = Value("Clip")
  val ClipRestoreElement: PrintElType.Value = Value("ClipRestore")
  val Bitmap: PrintElType.Value = Value("Bitmap")
  val Symbol: PrintElType.Value = Value("Symbol")
  val SymbolFiller: PrintElType.Value = Value("SymbolFiller")
  val DecoreMarker: PrintElType.Value = Value("Decore")
  val GraphBitmap: PrintElType.Value = Value("GraphBitmap")
  val RotationPrintElement: PrintElType.Value = Value("Rotation")
  val RotationEndPrintElement: PrintElType.Value = Value("RotationEnd")
}

class RotationPrintElement(px:Float,py:Float,angle:Float) extends PrintElement(new Rectangle2D.Float(px,py,angle,0f)){
  override def getElementType: PrintElType.Value = PrintElType.RotationPrintElement

  override def print(g: Graphics2D, ctx: RenderContext): Unit = {}
}

object RotationEndPrintElement extends NoBoundsPrintElement(PrintElType.RotationEndPrintElement) {
  override def print(g: Graphics2D, ctx: RenderContext): Unit = {}
}


abstract class ATextPrintElement(nbounds: Rectangle2D.Float, fontStyle: String) extends PrintElement(nbounds) {
  def text: String

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeUTF(text)
    out.writeUTF(fontStyle)
  }

  def getElementType:PrintElType.Value = PrintElType.TextField

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    val fStyle = ctx.fontStyleList.getStyle(fontStyle)
    val offset = fStyle.lineMetrics.getAscent
    val x: Float = ctx.toUnit(bounds.x)
    val y: Float = ctx.toUnit(bounds.y) + offset
    g.setFont(fStyle.font)
    g.setColor(fStyle.color)
    g.drawString(text, x, y)
    if (fStyle.underline) {
      val bounds = fStyle.font.getStringBounds(text, g.getFontRenderContext)
      g.draw(new Line2D.Float(x, y + offset * 0.15f, x + bounds.getWidth.toFloat, y + offset * 0.15f))
    }
  }
}


case class DimLinePrintElement(nbounds: Rectangle2D.Float, ncolor: Color, style: Int, mainRefPoint: VectorConstant,
                               relDist: Double, textScale: Double, points: IndexedSeq[DimensionPoint], mPoints: IndexedSeq[VectorConstant]) extends PrintElement(nbounds) {
  lazy val mainRefIntersection: VectorConstant = mline.intersectionWith(Line3D(mainRefPoint, hdirVector))
  lazy val intersectionLines: IndexedSeq[(DimensionPoint, VectorConstant)] = points.map(p => (p, mline.intersectionWith(Line3D(p.refPoint, hdirVector)))).sortBy(_._2)(VectorConstant.pointOrderingYFlip)
  lazy val mIntersectionPoints: IndexedSeq[VectorConstant] = mPoints.map(p => origLine.intersectionWith(Line3D(p, origHDirVector))).sorted(VectorConstant.pointOrdering /*YFlip*/)
  lazy val firstInterPoint: VectorConstant = if (intersectionLines.isEmpty) NULLVECTOR else intersectionLines.head._2
  lazy val lastInterPoint: VectorConstant = if (intersectionLines.isEmpty) NULLVECTOR else intersectionLines.last._2
  lazy val hitPoints: Seq[VectorConstant] = if (intersectionLines.isEmpty) Seq.empty else intersectionLines.map(_._2)
  val radAngle: Double = nbounds.width * math.Pi / 180d
  val mdirVector: VectorConstant = VectorConstant.fromAngle2D(-radAngle)
  val origDirVector: VectorConstant = VectorConstant.fromAngle2D(radAngle)
  val hdirVector = new VectorConstant(-mdirVector.y, mdirVector.x, 0)
  val origHDirVector = new VectorConstant(-origDirVector.y, origDirVector.x, 0)
  val position = new VectorConstant(nbounds.x, nbounds.y, 0)
  val mline: Line3D = Line3D(position, mdirVector)
  val origLine: Line3D = Line3D(position, origDirVector)

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(ncolor.getRGB)
    out.writeInt(style)
    mainRefPoint.write(out)
    out.writeDouble(relDist)
    out.writeDouble(textScale)
    DimensionPoint.writeList(points, out)
    out.writeInt(mPoints.size)
    for (mp <- mPoints) mp.write(out)
  }

  def getElementType: PrintElType.Value = PrintElType.DimLine

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    ctx.drawDimLine(g, this)
  }
}


case class SymbolPrintElement(nbounds: Rectangle2D.Float, symbolData: Reference, angle: Double, scale: Double, paramValues: String) extends
  PrintElement(nbounds) {
  lazy val pos = new VectorConstant(nbounds.getX, nbounds.getY, 0)

  def layerScale: Float = nbounds.width

  override def write(out: DataOutput): Unit = {
    super.write(out)
    symbolData.write(out)
    out.writeDouble(angle)
    out.writeDouble(scale)
    out.writeUTF(paramValues)
  }

  def getElementType: PrintElType.Value = PrintElType.Symbol

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    ctx.drawSymbol(g, this)
  }
}


case class SymbolFillerPrintElement(nbounds: Rectangle2D.Float, symbolData: Reference, angle: Double, scale: Double,
                                    paramValues: String, endPoint: VectorConstant, code: Int, value1: Double, value2: Double) extends
  PrintElement(nbounds) {
  lazy val startPoint = new VectorConstant(nbounds.getX, nbounds.getY, 0)

  def layerScale: Float = nbounds.width

  override def write(out: DataOutput): Unit = {
    super.write(out)
    symbolData.write(out)
    out.writeDouble(angle)
    out.writeDouble(scale)
    out.writeUTF(paramValues)
    endPoint.write(out)
    out.writeInt(code)
    out.writeDouble(value1)
    out.writeDouble(value2)
  }

  def getElementType:PrintElType.Value = PrintElType.SymbolFiller

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    ctx.drawSymbolFiller(g, this)
  }
}


class TextPrintElement(nbounds: Rectangle2D.Float, val text: String, fontStyle: String)
  extends ATextPrintElement(nbounds, fontStyle) {
}


class PlaceHolderElement(nbounds: Rectangle2D.Float, val name: String, fontStyle: String)
  extends ATextPrintElement(nbounds, fontStyle) {
  var value: String = ""

  def text: String = value
}


case class EllipsePrintElement(nbounds: Rectangle2D.Float, thick: Float, lineStyle: Byte, borderColor: Color,
                               mainAngle: Float, startAngle: Float, endAngle: Float) extends PrintElement(nbounds) {
  val r1: Float = bounds.width / 2
  val r2: Float = bounds.height / 2

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeFloat(thick)
    out.writeByte(lineStyle)
    out.writeInt(borderColor.getRGB)
    out.writeFloat(mainAngle)
    out.writeFloat(startAngle)
    out.writeFloat(endAngle)
  }

  def getElementType:PrintElType.Value = PrintElType.Ellipse

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    val centerX = bounds.x + r1
    val centerY = bounds.y + r2
    val sa = getInnerAngle(startAngle * math.Pi / 180d) * 180d / math.Pi
    val ea = getInnerAngle(endAngle * math.Pi / 180d) * 180d / math.Pi
    PrintElement.printArc.setArc(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.width), ctx.toUnit(bounds.height),
      sa, (if (ea < sa) 360 else 0) + ea - sa, Arc2D.OPEN)
    g.setColor(borderColor)
    g.setStroke(ctx.getStroke(thick, lineStyle))
    val af = AffineTransform.getRotateInstance(-mainAngle * math.Pi / 180d, ctx.toUnit(centerX), ctx.toUnit(centerY))
    val newArc = af.createTransformedShape(PrintElement.printArc)
    g.draw(newArc)
  }

  def getInnerAngle(outerAngle: Double): Double =
    if (outerAngle > math.Pi / 2) {
      if (outerAngle > PrintElement.PI_32) getInnerAngleFirstQuadrant(outerAngle - math.Pi * 2) + math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle - math.Pi) + math.Pi
    }
    else if (outerAngle < -math.Pi / 2) {
      if (outerAngle < -PrintElement.PI_32) getInnerAngleFirstQuadrant(outerAngle + math.Pi * 2) - math.Pi * 2 else getInnerAngleFirstQuadrant(outerAngle + math.Pi) - math.Pi
    }
    else getInnerAngleFirstQuadrant(outerAngle)

  private def getInnerAngleFirstQuadrant(outerAngle: Double) = math.atan(math.tan(outerAngle) * r1 / r2)
}


case class ArcPrintElement(nbounds: Rectangle2D.Float, thick: Float, lineStyle: Byte, borderColor: Color,
                           startAngle: Float, endAngle: Float) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeFloat(thick)
    out.writeByte(lineStyle)
    out.writeInt(borderColor.getRGB)
    out.writeFloat(startAngle)
    out.writeFloat(endAngle)
  }

  def getElementType:PrintElType.Value = PrintElType.Arc

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    PrintElement.printArc.setArc(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.width), ctx.toUnit(bounds.height),
      startAngle, (if (endAngle < startAngle) 360 else 0) + endAngle - startAngle, Arc2D.OPEN)
    g.setColor(borderColor)
    g.setStroke(ctx.getStroke(thick, lineStyle))
    g.draw(PrintElement.printArc)
  }
}


case class RectPrintElement(nbounds: Rectangle2D.Float, thick: Float, lineStyle: Byte, borderColor: Color) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeFloat(thick)
    out.writeByte(lineStyle)
    out.writeInt(borderColor.getRGB)
  }

  def getElementType:PrintElType.Value = PrintElType.Rect

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    //println("print rect clip:"+g.getClip+" Stroke:"+thick)
    PrintElement.printRect.setRect(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.width), ctx.toUnit(bounds.height))
    g.setColor(borderColor)
    g.setStroke(ctx.getStroke(thick, lineStyle))
    g.draw(PrintElement.printRect)
  }
}


case class FillPrintElement(nbounds: Rectangle2D.Float, foregroundColor: Color, hatchStyle: Int,
                            backgroundColor: Color) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(foregroundColor.getRGB)
    out.writeInt(hatchStyle)
    out.writeInt(backgroundColor.getRGB)
  }

  def getElementType:PrintElType.Value = PrintElType.Filler

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    PrintElement.printRect.setRect(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.width), ctx.toUnit(bounds.height))
    g.setColor(backgroundColor)
    g.fill(PrintElement.printRect)
  }
}


case class LinePrintElement(nbounds: Rectangle2D.Float, thick: Float, lineStyle: Byte, lineColor: Color) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeFloat(thick)
    out.writeByte(lineStyle)
    out.writeInt(lineColor.getRGB)
  }

  def getElementType:PrintElType.Value = PrintElType.Line

  def print(g: Graphics2D, ctx: RenderContext): Unit = if (thick != 0) {
    PrintElement.printLine.setLine(ctx.toUnit(bounds.x), ctx.toUnit(bounds.y), ctx.toUnit(bounds.x + bounds.width), ctx.toUnit(bounds.y + bounds.height))
    g.setColor(lineColor)
    g.setStroke(ctx.getStroke(thick, lineStyle))
    //g.setStroke(PrintElement.strokeMap(thick))
    g.draw(PrintElement.printLine)
  }
}


case class BitmapPrintElement(nbounds: Rectangle2D.Float, link: String) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeUTF(link)
  }

  def getElementType:PrintElType.Value = PrintElType.Bitmap

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    val file = new File(link)
    if (file.exists) {
      try {
        val image = ImageIO.read(file)
        g.drawImage(image, ctx.toUnit(bounds.x).toInt, ctx.toUnit(bounds.y).toInt, ctx.toUnit(bounds.width).toInt, ctx.toUnit(bounds.height).toInt, null)
      } catch {case NonFatal(e) => util.Log.e("when reading image :'" + link + "' " + e.getMessage, e)}
    }

  }
}


case class GraphBitmapPrintElement(nbounds: Rectangle2D.Float, link: String, angle: Double) extends PrintElement(nbounds) {
  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeUTF(link)
    out.writeDouble(angle)
  }

  def getElementType:PrintElType.Value = PrintElType.GraphBitmap

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    val file = new File(ctx.resolveImagePath(link))
    if (file.exists) {
      try {
        val image = ImageIO.read(file)
        val oldTrans = g.getTransform
        val x = ctx.toUnit(bounds.x).toInt
        val y = ctx.toUnit(bounds.height).toInt
        if (angle != 0d) g.rotate(-angle, x, y)
        g.drawImage(image, x, ctx.toUnit(bounds.y).toInt, ctx.toUnit(bounds.width).toInt, y, 0, 0, image.getWidth, image.getHeight, null)
        g.setTransform(oldTrans)
      } catch {case NonFatal(e) => util.Log.e("when reading image :'" + link + "' " + e.getMessage, e)}
    }

  }
}


case class GraphTextElement(nbounds: Rectangle2D.Float, text: String, fontName: String, style: Int, textAngle: Float, obligeAngle: Float, color: Color, lineSpace: Float) extends
  PrintElement(nbounds) {

  def getElementType:PrintElType.Value = PrintElType.GraphText

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeUTF(text)
    out.writeUTF(fontName)
    out.writeInt(style)
    out.writeFloat(textAngle)
    out.writeFloat(obligeAngle)
    out.writeInt(color.getRGB)
    out.writeFloat(lineSpace)
  }

  def print(g: Graphics2D, ctx: RenderContext): Unit = if (text.nonEmpty) {
    val clip = g.getClip
    val font = ctx.getGraphFont(fontName, ctx.toUnit(nbounds.height) * 25.4f / 72.0f, style)
    //println("Print graphElem :"+nbounds+" h:"+ctx.toUnit(nbounds.height)*25.4f/72.0f)
    val rtext = if ((style & PrintElement.capitalStyle) > 0) text.toUpperCase else text
    val layout = new TextLayout(rtext, font, g.getFontRenderContext)
    val testLayout = new TextLayout("AWqgÖ", font, FontStyle.fontRenderCtx)
    val textBounds = layout.getBounds.asInstanceOf[Rectangle2D.Float]
    val textWidth = if (nbounds.width == 0f) textBounds.width else ctx.toUnit(nbounds.width)
    val textHeight = testLayout.getBounds.getHeight.toFloat //lineMetrics.getAscent+math.abs(lineMetrics.getDescent)
    val alignXDelta = if ((style & PrintElement.orient_HCenter) > 0) -textWidth / 2 else if ((style & PrintElement.orient_Right) > 0) -textWidth else 0
    val alignYDelta = if ((style & 1) == 1) -textHeight / 2 else if ((style & 2) > 0) -textHeight else 0
    val radAngle = textAngle * math.Pi / 180f

    val tx = alignXDelta + textBounds.x
    val ty = -math.abs(testLayout.getDescent) - alignYDelta

    val tvx = new VectorConstant(tx * math.cos(radAngle), tx * math.sin(radAngle), 0)
    val tvy = new VectorConstant(ty * math.sin(radAngle), -ty * math.cos(radAngle), 0)
    //println("Print graph Element angle:"+textAngle+" "+radAngle)
    val xpos = (ctx.toUnit(bounds.x) + tvx.x + tvy.x).toFloat
    val ypos = (ctx.toUnit(bounds.y) - tvx.y - tvy.y).toFloat

    val oldTrans = g.getTransform

    if (textAngle != 0d) g.rotate(-radAngle, xpos, ypos) else if (nbounds.width > 0) g.clipRect(math.floor(xpos + textBounds.x - .5f).toInt, math.floor(ypos + textBounds.y).toInt,
      math.ceil(textWidth + 1f).toInt, math.ceil(textBounds.height + 1.5f).toInt)
    g.setPaint(color)
    if (rtext.contains('{')) { // subtext
      val (normalTexts, subTexts) = StringUtils.splitBracketed(rtext, '{', '}')
      val smallFont = ctx.getGraphFont(fontName, (ctx.toUnit(nbounds.height) * 25.4f / 72.0f) * .7f, style)
      var brxpos = 0f
      for (ix <- normalTexts.indices; ntext = normalTexts(ix)) {
        val nlayout = new TextLayout(ntext, font, g.getFontRenderContext)
        StringUtils.fillTextLayout(g, layout, xpos + brxpos, ypos)
        g.setPaint(color)
        nlayout.draw(g, xpos + brxpos, ypos)
        brxpos += nlayout.getAdvance + 1f
        if (ix < subTexts.size) {
          val slayout = new TextLayout(subTexts(ix), smallFont, g.getFontRenderContext)
          StringUtils.fillTextLayout(g, layout, xpos + brxpos, ypos + 1.5f)
          g.setPaint(color)
          slayout.draw(g, xpos + brxpos, ypos + 1.5f)
          brxpos += slayout.getAdvance + 0.5f
        }
      }
    } else {
      StringUtils.fillTextLayout(g, layout, xpos, ypos, wide = true)
      g.setPaint(color)
      try {
        layout.draw(g, xpos, ypos)
        if ((style & PrintElement.frameSquare) > 0) {
          g.draw(new Rectangle2D.Float(textBounds.x + xpos - 3, textBounds.y + ypos - 3, textBounds.width + 6, textBounds.height + 6))
        }
        else if ((style & PrintElement.frameRound) > 0) {
          val halfHeight = textBounds.height / 8
          g.draw(new Line2D.Float(textBounds.x + xpos - 2 + halfHeight, textBounds.y + ypos - 2,
            textBounds.x + xpos + textBounds.width - halfHeight, textBounds.y + ypos - 2))
          g.draw(new Line2D.Float(textBounds.x + xpos - 2 + halfHeight, textBounds.y + ypos + textBounds.height + 2,
            textBounds.x + xpos + textBounds.width - halfHeight, textBounds.y + ypos + textBounds.height + 2))
          g.draw(new Arc2D.Float(textBounds.x + xpos - 2 - textBounds.height / 2 + halfHeight, textBounds.y + ypos - 2, textBounds.height + 4, textBounds.height + 4, 90, 180, 0))
          g.draw(new Arc2D.Float(textBounds.x + xpos + textBounds.width - textBounds.height / 2 - halfHeight - 2, textBounds.y + ypos - 2, textBounds.height + 4, textBounds.height + 4, 270, 180, 0))
        }
      } catch {
        case e: Throwable => Log.e("Print text '" + text + "'" + e.getMessage, e)
      }
    }
    if (textAngle != 0d) g.setTransform(oldTrans) else g.setClip(clip)
  }
}


case class PolyPrintElement(textScale: Float, lineStyle: Byte, color: Color, hatchColor: Color, poly: Polygon, hatchStyle: Option[Int],
                            paperScale: Boolean, layerScale: Float, startPoint: Point2D.Float, hatchAngle: Double, name: String = "") extends PrintElement(poly.getBounds) {

  lazy val vStartPoint = new VectorConstant(startPoint.x, startPoint.y, 0)

  def this(in: DataInput) =
    this(in.readFloat, in.readByte, new Color(in.readInt), new Color(in.readInt), new Polygon(Nil, for (_ <- 0 until in.readInt) yield Polygon.readPointList(in)),
      if (in.readBoolean) Some(in.readInt) else None, in.readBoolean, in.readFloat, new Point2D.Float(in.readFloat, in.readFloat), in.readDouble, in.readUTF)

  //println("Create Poly thick"+thick+" color:"+fillColor+" points:"+poly.pathList.foldLeft(0)(_+_.points.size))
  override def write(out: DataOutput): Unit = {
    out.writeByte(getElementType.id.toByte)
    out.writeFloat(textScale)
    out.writeByte(lineStyle)
    out.writeInt(color.getRGB)
    out.writeInt(hatchColor.getRGB)
    out.writeInt(poly.pathList.size)
    poly.pathList.foreach(_.write(out))
    out.writeBoolean(hatchStyle.isDefined)
    for (h <- hatchStyle)
      out.writeInt(h)
    out.writeBoolean(paperScale)
    out.writeFloat(layerScale)
    out.writeFloat(startPoint.x)
    out.writeFloat(startPoint.y)
    out.writeDouble(hatchAngle)
    out.writeUTF(name)
  }

  def getElementType:PrintElType.Value = PrintElType.Poly

  def print(g: Graphics2D, ctx: RenderContext): Unit = {
    def transform(v: VectorConstant) = new VectorConstant(ctx.toUnit(v.x), ctx.toUnit(v.y), 0)

    val theArea = PolygonToJavaArea.toPathTransformed(poly,transform)
    /*if (color != Color.white) {*/
      g.setColor(color)
      g.fill(theArea)
    //}
    for (hs <- hatchStyle)
      ctx.drawHatch(poly, hs, paperScale, g, hatchColor, layerScale, vStartPoint, hatchAngle)
    if (name.trim.nonEmpty) {
      val strings = name.split("\n").map(_.trim)
      val mostWideString = strings.maxBy(_.length)
      val midPoint = transform(Polygon.midOfPointList(poly.pathList))
      g.setColor(Color.BLACK)
      val tScale = if (textScale == 0) 1f else textScale
      g.setFont(PrintElement.tinyFont.deriveFont(tScale * PrintElement.tinyFont.getSize.toFloat))
      val metric = g.getFontMetrics.getStringBounds(mostWideString, g)
      val w = metric.getWidth
      val h = metric.getHeight + 1
      if (hatchAngle != 0) {
        val oldTrans = g.getTransform
        g.rotate(-hatchAngle / 180d * Math.PI, midPoint.x.toFloat, midPoint.y.toFloat)

        for (i <- strings.indices; st = strings(i))
          g.drawString(st, midPoint.x.toFloat - w.toFloat / 2f, midPoint.y.toFloat + (h * i).toFloat)
        g.setTransform(oldTrans)
      } else for (i <- strings.indices; st = strings(i))
        g.drawString(st, midPoint.x.toFloat - w.toFloat / 2f, midPoint.y.toFloat + (h * i).toFloat)
    }
  }
}


object PrintElement {
  val printRect = new Rectangle2D.Float
  val printArc = new Arc2D.Float
  val printLine = new Line2D.Float
  val orient_Right = 16
  val orient_HCenter = 8
  val frameRound = 1024
  val frameSquare = 2048
  val capitalStyle = 4096
  val PI_32: Double = math.Pi * 3 / 2
  val tinyFont = new Font("Arial", 0, 10)
  val underlineStroke = new BasicStroke()



  def apply(in: DataInput): PrintElement = PrintElType(in.readByte) match {
    case PrintElType.TextField => new TextPrintElement(readBounds(in), in.readUTF, in.readUTF)
    case PrintElType.Rect => RectPrintElement(readBounds(in), in.readFloat, in.readByte, new Color(in.readInt))
    case PrintElType.Line => LinePrintElement(readBounds(in), in.readFloat, in.readByte, new Color(in.readInt))
    case PrintElType.Arc => ArcPrintElement(readBounds(in), in.readFloat, in.readByte, new Color(in.readInt), in.readFloat, in.readFloat)
    case PrintElType.Ellipse => EllipsePrintElement(readBounds(in), in.readFloat, in.readByte, new Color(in.readInt), in.readFloat, in.readFloat, in.readFloat)
    case PrintElType.Poly => new PolyPrintElement(in)
    case PrintElType.GraphText => GraphTextElement(readBounds(in), in.readUTF, in.readUTF, in.readInt, in.readFloat, in.readFloat, new Color(in.readInt), in.readFloat)
    case PrintElType.Filler => FillPrintElement(readBounds(in), new Color(in.readInt), in.readInt, new Color(in.readInt))
    case PrintElType.DimLine =>
      DimLinePrintElement(readBounds(in), new Color(in.readInt), in.readInt, Expression.readConstant(in).toVector,
        in.readDouble, in.readDouble, DimensionPoint.readList(in), for (_ <- 0 until in.readInt) yield Expression.readConstant(in).toVector)
    case PrintElType.ClipPrintElement => new ClipPrintElement(readBounds(in))
    case PrintElType.ClipRestoreElement => new RestoreClipPrintElement
    case PrintElType.Bitmap => BitmapPrintElement(readBounds(in), in.readUTF)
    case PrintElType.Symbol => SymbolPrintElement(readBounds(in), Reference(in), in.readDouble, in.readDouble, in.readUTF)
    case PrintElType.SymbolFiller => SymbolFillerPrintElement(readBounds(in), Reference(in), in.readDouble, in.readDouble, in.readUTF,
      VectorConstant.read(in), in.readInt, in.readDouble, in.readDouble)
    case PrintElType.DecoreMarker => DecoreMarkerElement
    case PrintElType.GraphBitmap => GraphBitmapPrintElement(readBounds(in), in.readUTF, in.readDouble())
    case PrintElType.PageBreak => PageBreakMarker
    case PrintElType.RotationPrintElement => val b=readBounds(in); new RotationPrintElement(b.x,b.y,b.width)
    case PrintElType.RotationEndPrintElement=> RotationEndPrintElement
    case other => throw new IllegalArgumentException("Unknown PrintElement-Type:" + other)
  }

  def readBounds(in: DataInput) = new Rectangle2D.Float(in.readFloat, in.readFloat, in.readFloat, in.readFloat)
}


