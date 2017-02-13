package definition.data
import definition.expression.VectorConstant
import java.awt.Graphics2D
import java.awt.font.TextLayout
import definition.expression.Polygon
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.font.TextAttribute
import java.awt.Shape

trait RenderContext  {  
  
	val strokeMap=collection.mutable.HashMap[(Int),BasicStroke]()
	
	var oldClip:Option[Shape]=None
	
	def styleIsBold(style:Int)= (style & FontStyle.boldStyle)>0
	def styleIsItalic(style:Int)=(style &FontStyle.italicStyle)>0
	def styleIsUnderline(style:Int)=(style & 512)>0
	def getFontStyle(style:Int)= (if(styleIsBold(style)) Font.BOLD else 0) + (if(styleIsItalic(style)) Font.ITALIC else 0)  
  
  private val graphFontCache=collection.mutable.HashMap[(String,Double,Int),Font]()
	def lineStyleHandler:AbstractLineStyleHandler
	  
	def getStroke(thick:Float,style:Int)={	  
	  val key=thick.hashCode+style.toShort*Short.MaxValue	 
	  strokeMap.getOrElseUpdate(key,lineStyleHandler.createStroke(toUnit(1f),thick*100f/toUnit(1f),style))	  
	} 
  
	def getScale:Double
	def toUnit(mm:Double):Float = (mm*72.0f/25.4f).toFloat
	def fontStyleList:FontStyleList	
	def fromMM(value:Float):Int =	toUnit(value).toInt
	def drawHatch(poly:Polygon,hatchStyle:Int,paperScale:Boolean,g:Graphics2D,color:Color,layerScale:Float,startPoint:VectorConstant,hatchAngle:Double) 
	
	def getGraphFont(fontFamily:String,height:Double,style:Int):Font= {
	  val key=(fontFamily,height,style)
	  if(graphFontCache.contains(key)) graphFontCache(key)
	  else {
	    val hFont={
	      val afont=new Font(fontFamily,getFontStyle(style),100)
	      if(styleIsUnderline(style)) {
					  val attrMap = new java.util.Hashtable[TextAttribute,Object]()
			      attrMap.put(TextAttribute.UNDERLINE,TextAttribute.UNDERLINE_LOW_ONE_PIXEL)
			      afont.deriveFont(attrMap)  
					}
	      else afont
	    }
	    
	    val fm=hFont.getLineMetrics("AqgÖ",FontStyle.fontRenderCtx)
	    //println("Get Font:"+fontFamily+" height:"+height+" asc:"+asc+" desc:"+fm.getDescent+" "+fm.getBaselineOffsets.mkString("|")+" >"+fm.getBaselineIndex)
	    val tl=new TextLayout("AqgÖ",hFont,FontStyle.fontRenderCtx)
	    val asc=tl.getAscent
	    val resHeight=(height*72.0f/25.4f).toFloat
	    //println("TextLayout "+fontFamily+" ascent:"+tl.getAscent+" descent:"+tl.getDescent+" bounds:"+tl.getBounds)
	    val newFont=hFont.deriveFont(resHeight *100f/asc)
	    graphFontCache(key)=newFont
	    newFont
	  }
	}
	
	def drawDimLine(g:Graphics2D,d:DimLinePrintElement):Unit	
	
	def drawSymbol(g:Graphics2D,s:SymbolPrintElement):Unit
  
  def drawSymbolFiller(g:Graphics2D,s:SymbolFillerPrintElement):Unit
}