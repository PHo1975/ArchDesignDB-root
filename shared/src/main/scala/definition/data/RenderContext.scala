package definition.data

import java.awt._
import java.awt.font.{TextAttribute, TextLayout}

import definition.expression.{Polygon, VectorConstant}

import scala.collection.mutable

trait RenderContext {

  val strokeMap: mutable.HashMap[Int, BasicStroke] = collection.mutable.HashMap[Int, BasicStroke]()
  private val graphFontCache = collection.mutable.HashMap[(String, Double, Int), Font]()
  var oldClip: Option[Shape] = None

  def lineStyleHandler: AbstractLineStyleHandler

  def getStroke(thick: Float, style: Int): BasicStroke = {
    val key = thick.hashCode + style.toShort * Short.MaxValue
    strokeMap.getOrElseUpdate(key, lineStyleHandler.createStroke(toUnit(1f), thick * 100f / toUnit(1f), style))
  }

  def getScale: Double

  def fontStyleList: FontStyleList

  def fromMM(value: Float): Int = toUnit(value).toInt

  def toUnit(mm: Double): Float = (mm * 72.0f / 25.4f).toFloat

  def drawHatch(poly: Polygon, hatchStyle: Int, paperScale: Boolean, g: Graphics2D, color: Color, layerScale: Float, startPoint: VectorConstant, hatchAngle: Double):Unit

  def getGraphFont(fontFamily: String, height: Double, style: Int): Font = {
    val key = (fontFamily, height, style)
    if (graphFontCache.contains(key)) graphFontCache(key)
    else {
      val hFont = {
        val afont = new Font(fontFamily, getFontStyle(style), 100)
        if (styleIsUnderline(style)) {
          val attrMap = new java.util.Hashtable[TextAttribute, Object]()
          attrMap.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_LOW_ONE_PIXEL)
          afont.deriveFont(attrMap)
        }
        else afont
      }

      //val fm = hFont.getLineMetrics("AqgÖ", FontStyle.fontRenderCtx)
      //println("Get Font:"+fontFamily+" height:"+height+" asc:"+asc+" desc:"+fm.getDescent+" "+fm.getBaselineOffsets.mkString("|")+" >"+fm.getBaselineIndex)
      val tl = new TextLayout("AqgÖ", hFont, FontStyle.fontRenderCtx)
      val asc = tl.getAscent
      val resHeight = (height * 72.0f / 25.4f).toFloat
      //println("TextLayout "+fontFamily+" ascent:"+tl.getAscent+" descent:"+tl.getDescent+" bounds:"+tl.getBounds)
      val newFont = hFont.deriveFont(resHeight * 100f / asc)
      graphFontCache(key) = newFont
      newFont
    }
  }

  def styleIsUnderline(style: Int): Boolean = (style & 512) > 0

  def getFontStyle(style: Int): Int = (if (styleIsBold(style)) Font.BOLD else 0) + (if (styleIsItalic(style)) Font.ITALIC else 0)

  def styleIsBold(style: Int): Boolean = (style & FontStyle.boldStyle) > 0

  def styleIsItalic(style: Int): Boolean = (style & FontStyle.italicStyle) > 0

  def drawDimLine(g: Graphics2D, d: DimLinePrintElement): Unit

  def drawSymbol(g: Graphics2D, s: SymbolPrintElement): Unit

  def drawSymbolFiller(g: Graphics2D, s: SymbolFillerPrintElement): Unit

  def resolveImagePath(path:String):String
}