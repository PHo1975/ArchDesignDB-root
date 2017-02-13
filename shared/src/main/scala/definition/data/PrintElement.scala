/**
 * Author: Peter Started:27.12.2010
 */
package definition.data

import java.awt.{BasicStroke, Color, Graphics2D,Font}
import java.awt.font.TextLayout
import java.awt.geom.{AffineTransform, Arc2D, Area, Line2D, Point2D, Rectangle2D}
import java.io.{DataInput, DataOutput, File}
import javax.imageio.ImageIO

import definition.expression.{Expression, Line3D, NULLVECTOR, Polygon, VectorConstant}
import util.StringUtils

import scala.util.control.NonFatal
import util.Log

/**
 * 
 */
abstract class PrintElement(val bounds:Rectangle2D.Float) {
	
	def write(out:DataOutput)= {
		out.writeByte(getElementType.id.toByte)
		out.writeFloat(bounds.x)
		out.writeFloat(bounds.y)
		out.writeFloat(bounds.width)
		out.writeFloat(bounds.height)
	}	
	def getElementType:PrintElType.Value
	
	def print(g:Graphics2D,ctx:RenderContext)
}

abstract class NoBoundsPrintElement(val getElementType:PrintElType.Value) extends PrintElement(null){
  override def write(out:DataOutput)= {
    out.writeByte(getElementType.id.toByte)
  }
}

object PageBreakMarker extends NoBoundsPrintElement(PrintElType.PageBreak){
  def print(g:Graphics2D,ctx:RenderContext)={}
}


class ClipPrintElement(nbounds:Rectangle2D.Float) extends PrintElement(nbounds){
   def getElementType=PrintElType.ClipPrintElement
  def print(g:Graphics2D,ctx:RenderContext)={
     ctx.oldClip=Option(g.getClip)
     g.clip(new Rectangle2D.Float(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height)))
   }
}

class RestoreClipPrintElement() extends NoBoundsPrintElement(PrintElType.ClipRestoreElement){   
  def print(g:Graphics2D,ctx:RenderContext)= for(cl<-ctx.oldClip){
    g.setClip(cl)
  }     
}


object DecoreMarkerElement extends NoBoundsPrintElement(PrintElType.DecoreMarker){
  def print(g:Graphics2D,ctx:RenderContext)={}
}

object PrintElType extends Enumeration {
	val TextField=Value("TextElement")
	val Line=Value("LineElement")
	val TextArea=Value("TextArea")
	val Rect=Value("RectElement")
	val Arc=Value("ArcElement")
	val Poly=Value("PolyElement")
	val Ellipse=Value("Ellipse")
	val GraphText=Value("GraphText")
	val Filler=Value("Filler")
	val DimLine=Value("DimLineElement")
	val PageBreak=Value("PageBreak")
	val ClipPrintElement=Value("Clip")
	val ClipRestoreElement=Value("ClipRestore")
	val Bitmap=Value("Bitmap")
  val Symbol=Value("Symbol")
  val SymbolFiller=Value("SymbolFiller")
  val DecoreMarker=Value("Decore")
	val GraphBitmap=Value("GraphBitmap")
}



abstract class ATextPrintElement(nbounds:Rectangle2D.Float,fontStyle:String) extends PrintElement(nbounds) {
	def text:String
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeUTF(text)
		out.writeUTF(fontStyle)		
	}
	def getElementType= PrintElType.TextField	
	
	def print(g:Graphics2D,ctx:RenderContext) = {		
	  //val clip=g.getClip()
		val fStyle=ctx.fontStyleList.getStyle(fontStyle)		
		val offset=fStyle.lineMetrics.getAscent
		val x=ctx.toUnit(bounds.x)
		val y=ctx.toUnit(bounds.y)+offset		
		g.setFont(fStyle.font)
		g.setColor(fStyle.color)
		//g.clipRect(ctx.toUnit(bounds.x).toInt, ctx.toUnit(bounds.y).toInt, ctx.toUnit(bounds.width).toInt+1,ctx.toUnit(bounds.height).toInt+1)
		g.drawString(text, x,y)
		//g.setClip(clip)
	}
}


case class DimLinePrintElement(nbounds:Rectangle2D.Float,ncolor:Color,style:Int, mainRefPoint:VectorConstant,
															 relDist:Double, textScale:Double, points:IndexedSeq[DimensionPoint], mPoints:IndexedSeq[VectorConstant] ) extends PrintElement(nbounds){
	val radAngle=nbounds.width*math.Pi/180d
	val mdirVector=VectorConstant.fromAngle2D(-radAngle)
	val origDirVector=VectorConstant.fromAngle2D(radAngle)
	
	val hdirVector=new VectorConstant(-mdirVector.y,mdirVector.x,0)
	val origHDirVector=new VectorConstant(-origDirVector.y,origDirVector.x,0)
	val position=new VectorConstant(nbounds.x,nbounds.y,0)
	val mline=new Line3D(position,mdirVector)
	val origLine=new Line3D(position,origDirVector)
	lazy val mainRefIntersection=mline.intersectionWith(new Line3D(mainRefPoint,hdirVector))
	lazy val intersectionLines=points.map(p=>(p,mline.intersectionWith(new Line3D(p.refPoint,hdirVector)))).sortBy(_._2)(VectorConstant.pointOrderingYFlip)
	lazy val mIntersectionPoints=mPoints.map(p=>origLine.intersectionWith(new Line3D(p,origHDirVector))).sorted(VectorConstant.pointOrdering/*YFlip*/)	
	lazy val firstInterPoint=if(intersectionLines.isEmpty) NULLVECTOR else intersectionLines.head._2
	lazy val lastInterPoint=if(intersectionLines.isEmpty) NULLVECTOR else intersectionLines.last._2	
	lazy val hitPoints=if(intersectionLines.isEmpty) Seq.empty else intersectionLines.map (_._2)

  override def write(out:DataOutput)= {
    super.write(out)
    out.writeInt(ncolor.getRGB)
    out.writeInt(style)
    mainRefPoint.write(out)
    out.writeDouble(relDist)
    out.writeDouble(textScale)
    DimensionPoint.writeList(points,out)
    out.writeInt(mPoints.size)
    for(mp<-mPoints)mp.write(out)
  }
  def getElementType= PrintElType.DimLine
  
	def print(g:Graphics2D,ctx:RenderContext) = {
    ctx.drawDimLine(g,this)
  }
}


case class SymbolPrintElement(nbounds:Rectangle2D.Float,symbolData:Reference,angle:Double,scale:Double,paramValues:String) extends
   PrintElement(nbounds) {  
  lazy val pos=new VectorConstant(nbounds.getX,nbounds.getY,0)
  def layerScale=nbounds.width
  
  override def write(out:DataOutput)= {
    super.write(out)
    symbolData.write(out)
    out.writeDouble(angle)
    out.writeDouble(scale)    
    out.writeUTF(paramValues)
  }
  
  def getElementType= PrintElType.Symbol
  def print(g:Graphics2D,ctx:RenderContext) = { 
    ctx.drawSymbol(g,this)
  }
}

case class SymbolFillerPrintElement(nbounds:Rectangle2D.Float,symbolData:Reference,angle:Double,scale:Double,
    paramValues:String,endPoint:VectorConstant,code:Int,value1:Double,value2:Double) extends
   PrintElement(nbounds) {  
  lazy val startPoint=new VectorConstant(nbounds.getX,nbounds.getY,0)
  def layerScale=nbounds.width
  
  override def write(out:DataOutput)= {
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
  
  def getElementType= PrintElType.SymbolFiller
  def print(g:Graphics2D,ctx:RenderContext) = { 
    ctx.drawSymbolFiller(g,this)
  }
}

class TextPrintElement(nbounds:Rectangle2D.Float,val text:String,fontStyle:String) extends ATextPrintElement(nbounds,fontStyle) {	
}


class PlaceHolderElement(nbounds:Rectangle2D.Float,val name:String,fontStyle:String) extends ATextPrintElement(nbounds,fontStyle) {	
	var value:String=""
	def text=value
}


case class EllipsePrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,borderColor:Color,mainAngle:Float,startAngle:Float,endAngle:Float) extends PrintElement(nbounds) {
	val r1=bounds.width/2
  val r2=bounds.height/2
	
	private def getInnerAngleFirstQuadrant(outerAngle:Double)= math.atan(math.tan(outerAngle)*r1/r2)
	
	def getInnerAngle(outerAngle:Double)= 
    if(outerAngle>math.Pi/2 ) {
      if(outerAngle>PrintElement.PI_32) getInnerAngleFirstQuadrant(outerAngle-math.Pi*2)+math.Pi*2 else getInnerAngleFirstQuadrant(outerAngle-math.Pi)+math.Pi 
      }
    else if(outerAngle< -math.Pi/2) { 
      if(outerAngle< -PrintElement.PI_32) getInnerAngleFirstQuadrant(outerAngle+math.Pi*2)-math.Pi*2  else  getInnerAngleFirstQuadrant(outerAngle+math.Pi)-math.Pi 
      }
    else getInnerAngleFirstQuadrant(outerAngle)
  
  override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(borderColor.getRGB)
		out.writeFloat(mainAngle)
		out.writeFloat(startAngle)
		out.writeFloat(endAngle)
	}
	def getElementType= PrintElType.Ellipse
	def print(g:Graphics2D,ctx:RenderContext) = {    
    val centerX=bounds.x+r1
    val centerY=bounds.y+r2
	  val sa=getInnerAngle(startAngle*math.Pi/180d)*180d/math.Pi		
		val ea=getInnerAngle(endAngle*math.Pi/180d)*180d/math.Pi
	  PrintElement.printArc.setArc(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height),
	      sa, (if(ea<sa)360 else 0)+ea-sa,Arc2D.OPEN)	  
	  g.setColor(borderColor)
	  g.setStroke(ctx.getStroke(thick,lineStyle))
	  val af=AffineTransform.getRotateInstance(-mainAngle*math.Pi/180d,ctx.toUnit(centerX),ctx.toUnit(centerY))
	  val newArc=af.createTransformedShape(PrintElement.printArc)		
		g.draw(newArc)		
	}
}



case class ArcPrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,borderColor:Color,startAngle:Float,endAngle:Float) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(borderColor.getRGB)
		out.writeFloat(startAngle)
		out.writeFloat(endAngle)
	}
	def getElementType= PrintElType.Arc	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  PrintElement.printArc.setArc(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height),
	      startAngle, (if (endAngle < startAngle) 360 else 0) + endAngle - startAngle,Arc2D.OPEN)
	  g.setColor(borderColor)
	  g.setStroke(ctx.getStroke(thick,lineStyle))
		g.draw(PrintElement.printArc)		
	}
}

case class RectPrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,borderColor:Color) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(borderColor.getRGB)
	}
	def getElementType= PrintElType.Rect 	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  PrintElement.printRect.setRect(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height))
	  g.setColor(borderColor)
	   g.setStroke(ctx.getStroke(thick,lineStyle))
		g.draw(PrintElement.printRect)		
	}
}

case class FillPrintElement(nbounds:Rectangle2D.Float,foregroundColor:Color,hatchStyle:Int,backgroundColor:Color) extends PrintElement(nbounds) {
  override def write(out:DataOutput)= {
		super.write(out)
		out.writeInt(foregroundColor.getRGB)
		out.writeInt(hatchStyle)
		out.writeInt(backgroundColor.getRGB)
	}
	def getElementType= PrintElType.Filler 	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  PrintElement.printRect.setRect(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.width),ctx.toUnit(bounds.height))
	  g.setColor(backgroundColor)	   
		g.fill(PrintElement.printRect)		
	}
}


case class LinePrintElement(nbounds:Rectangle2D.Float,thick:Float,lineStyle:Byte,lineColor:Color) extends PrintElement(nbounds) {
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeFloat(thick)
		out.writeByte(lineStyle)
		out.writeInt(lineColor.getRGB)
	}
	def getElementType= PrintElType.Line 	
	def print(g:Graphics2D,ctx:RenderContext) = if(thick!=0){
	  PrintElement.printLine.setLine(ctx.toUnit(bounds.x),ctx.toUnit(bounds.y),ctx.toUnit(bounds.x+bounds.width),ctx.toUnit(bounds.y+bounds.height))
	  g.setColor(lineColor)
	   g.setStroke(ctx.getStroke(thick,lineStyle))
	  //g.setStroke(PrintElement.strokeMap(thick))	  
		g.draw(PrintElement.printLine)		
	} 
}

case class BitmapPrintElement(nbounds:Rectangle2D.Float,link:String) extends PrintElement(nbounds){
  override def write(out:DataOutput)= {
		super.write(out)
		out.writeUTF(link)		
	}
	def getElementType= PrintElType.Bitmap 	
	def print(g:Graphics2D,ctx:RenderContext) = {
	  val file=new File(link)
	  if(file.exists){
	    try {
		    val image=ImageIO.read(file)
		    g.drawImage(image,ctx.toUnit(bounds.x).toInt,ctx.toUnit(bounds.y).toInt,ctx.toUnit(bounds.width).toInt,ctx.toUnit(bounds.height).toInt,null)
	    } catch {case NonFatal(e) => util.Log.e("when reading image :'"+link.toString+ "' "+e.getMessage,e )}
	  }
				
	}
}


case class GraphBitmapPrintElement(nbounds:Rectangle2D.Float,link:String,angle:Double) extends PrintElement(nbounds){
	override def write(out:DataOutput)= {
		super.write(out)
		out.writeUTF(link)
		out.writeDouble(angle)
	}
	def getElementType= PrintElType.GraphBitmap
	def print(g:Graphics2D,ctx:RenderContext) = {
		val file=new File(link)
		if(file.exists){
			try {
				val image=ImageIO.read(file)
				val oldTrans=g.getTransform
				val x= ctx.toUnit(bounds.x).toInt
				val y=ctx.toUnit(bounds.height).toInt
				if(angle!=0d) g.rotate(-angle,x,y)
				g.drawImage(image,x,ctx.toUnit(bounds.y).toInt,ctx.toUnit(bounds.width).toInt,y,0,0,image.getWidth,image.getHeight,null)
				g.setTransform(oldTrans)
			} catch {case NonFatal(e) => util.Log.e("when reading image :'"+link.toString+ "' "+e.getMessage,e )}
		}

	}
}



case class GraphTextElement(nbounds:Rectangle2D.Float,text:String,fontName:String,style:Int,textAngle:Float,obligeAngle:Float,color:Color,lineSpace:Float) extends
PrintElement(nbounds) {
  
  def getElementType=PrintElType.GraphText
  
  
  override def write(out:DataOutput)= {
		super.write(out)
		out.writeUTF(text)
		out.writeUTF(fontName)
		out.writeInt(style)
		out.writeFloat(textAngle)
		out.writeFloat(obligeAngle)
		out.writeInt(color.getRGB)
		out.writeFloat(lineSpace)
  }
  
  def print(g:Graphics2D,ctx:RenderContext) = if(text.length>0){
		val clip=g.getClip
		val font=ctx.getGraphFont(fontName,ctx.toUnit(nbounds.height)*25.4f/72.0f,style)
		//println("Print graphElem :"+nbounds+" h:"+ctx.toUnit(nbounds.height)*25.4f/72.0f)
		val layout=new TextLayout(text,font,g.getFontRenderContext)
		val testLayout=new TextLayout("AWqgÖ",font,FontStyle.fontRenderCtx)
		val textBounds=layout.getBounds.asInstanceOf[Rectangle2D.Float]
		val textWidth=if(nbounds.width==0f)textBounds.width else ctx.toUnit(nbounds.width)
		val textHeight=testLayout.getBounds.getHeight.toFloat //lineMetrics.getAscent+math.abs(lineMetrics.getDescent)
		val alignXDelta= if((style & PrintElement.orient_Center) >0) -textWidth/2 else if((style & PrintElement.orient_Right)>0) -textWidth else 0
		val alignYDelta= if((style & 1) ==1) -textHeight/2 else if((style & 2)>0) -textHeight else 0
		val radAngle=textAngle*math.Pi/180f

		val tx=alignXDelta+textBounds.x
		val ty= -math.abs(testLayout.getDescent)-alignYDelta

		val tvx=new VectorConstant(tx*math.cos(radAngle),tx*math.sin(radAngle),0)
		val tvy=new VectorConstant(ty*math.sin(radAngle),-ty*math.cos(radAngle),0)
		//println("Print graph Element angle:"+textAngle+" "+radAngle)
		val xpos=(ctx.toUnit(bounds.x)+tvx.x+tvy.x).toFloat
		val ypos=(ctx.toUnit(bounds.y)-tvx.y-tvy.y).toFloat

		val oldTrans=g.getTransform

		if(textAngle!=0d) g.rotate(-radAngle,xpos,ypos) else if(nbounds.width>0) g.clipRect(math.floor(xpos+textBounds.x-.5f).toInt,math.floor(ypos+textBounds.y).toInt,
			math.ceil(textWidth+1f).toInt,math.ceil(textBounds.height+1.5f).toInt)
		g.setPaint(color)
		if(text.contains('{')){ // subtext
		val (normalTexts,subTexts)= StringUtils.splitBracketed(text, '{','}')
			val smallFont=ctx.getGraphFont(fontName,(ctx.toUnit(nbounds.height)*25.4f/72.0f)*.7f,style)
			var brxpos=0f
			for(ix<-normalTexts.indices;ntext=normalTexts(ix)){
				val nlayout=new TextLayout(ntext,font,g.getFontRenderContext)
				StringUtils.fillTextLayout(g, layout, xpos+brxpos, ypos)
				g.setPaint(color)
				nlayout.draw(g,xpos+brxpos,ypos)
				brxpos+=nlayout.getAdvance+1f
				if(ix<subTexts.size ) {
					val slayout=new TextLayout(subTexts(ix),smallFont,g.getFontRenderContext)
					StringUtils.fillTextLayout(g, layout, xpos+brxpos,ypos+1.5f)
					g.setPaint(color)
					slayout.draw(g,xpos+brxpos,ypos+1.5f)
					brxpos+=slayout.getAdvance+0.5f
				}
			}
		} else{
			StringUtils.fillTextLayout(g, layout, xpos, ypos)
			g.setPaint(color)
			try {
				layout.draw(g, xpos, ypos)
			} catch {
				case e:Throwable=> Log.e("Print text '"+text+"'" +e.getMessage,e)
			}
		}
		if(textAngle!=0d) g.setTransform(oldTrans) else g.setClip(clip)
	}
}


case class PolyPrintElement(textScale:Float,lineStyle:Byte,color:Color,fillColor:Color,poly:Polygon,hatchStyle:Option[Int],
    paperScale:Boolean,layerScale:Float,startPoint:Point2D.Float,hatchAngle:Double,name:String="") extends PrintElement(poly.getBounds) {   
  
  def this(in:DataInput) = 
    this(in.readFloat,in.readByte,new Color(in.readInt),new Color(in.readInt),new Polygon(Nil,for(i<-0 until in.readInt) yield Polygon.readPointList(in)),
        if(in.readBoolean) Some(in.readInt) else None,in.readBoolean,in.readFloat,new Point2D.Float(in.readFloat,in.readFloat),in.readDouble,in.readUTF)  
  
  lazy val vStartPoint=new VectorConstant(startPoint.x,startPoint.y,0)
  //println("Create Poly thick"+thick+" color:"+fillColor+" points:"+poly.pathList.foldLeft(0)(_+_.points.size))      
  override def write(out:DataOutput)= {
    out.writeByte(getElementType.id.toByte)
    out.writeFloat(textScale)
    out.writeByte(lineStyle)
    out.writeInt(color.getRGB)
    out.writeInt(fillColor.getRGB)
    out.writeInt(poly.pathList.size)
    poly.pathList.foreach(_.write(out))
    out.writeBoolean(hatchStyle.isDefined)
    for(h<-hatchStyle)
      out.writeInt(h)
    out.writeBoolean(paperScale)
    out.writeFloat(layerScale)
    out.writeFloat(startPoint.x)
    out.writeFloat(startPoint.y)
    out.writeDouble(hatchAngle)
    out.writeUTF(name)    
  }
  
  def getElementType= PrintElType.Poly
  
  def print(g:Graphics2D,ctx:RenderContext) = {
    def transform(v:VectorConstant)=	new VectorConstant(ctx.toUnit(v.x),ctx.toUnit(v.y),0)
	  
	  val newPoly=poly.toPathTransformed(transform)
	  val theArea=new Area(newPoly)
    if(color!= Color.white) {
    	g.setColor(color)
    	g.fill(theArea)
    }
		for(hs<-hatchStyle)
		  ctx.drawHatch(poly,hs,paperScale,g,if(hs!=0) Color.black else fillColor,layerScale,vStartPoint,hatchAngle)
		if(name.trim.length>0){
		  val strings=name.split("\n").map(_.trim)
	    val mostWideString=strings.maxBy(_.length)
		  val midPoint=transform(Polygon.midOfPointList(poly.pathList))	  
	    g.setColor(Color.BLACK)
      val tScale=if(textScale==0) 1f else textScale
	    g.setFont(PrintElement.tinyFont.deriveFont(tScale*PrintElement.tinyFont.getSize.toFloat))
	    val metric=g.getFontMetrics.getStringBounds(mostWideString,g)
	    val w=metric.getWidth
	    val h=metric.getHeight+1
	    if(hatchAngle!=0) {
	      val oldTrans=g.getTransform
	      g.rotate(-hatchAngle/180d*Math.PI,midPoint.x.toFloat,midPoint.y.toFloat)
        
	      for(i<-strings.indices;st=strings(i))
	        g.drawString(st, midPoint.x.toFloat-w.toFloat/2f, midPoint.y.toFloat+(h*i).toFloat) 
	      g.setTransform(oldTrans)
	    } else for(i<-strings.indices;st=strings(i))
	    g.drawString(st, midPoint.x.toFloat-w.toFloat/2f, midPoint.y.toFloat+(h*i).toFloat) 
		}		
  }
  
}


object PrintElement {
	val printRect=new Rectangle2D.Float
	val printArc=new Arc2D.Float
	val printLine=new Line2D.Float
	val orient_Right=16
	val orient_Center=8	
	val PI_32=math.Pi*3/2
	val tinyFont=new Font("Arial",0,10)
  
	def apply(in:DataInput) = PrintElType(in.readByte) match {
		case PrintElType.TextField => new TextPrintElement(readBounds(in),in.readUTF,in.readUTF)
		case PrintElType.Rect => RectPrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt))
		case PrintElType.Line => LinePrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt))
		case PrintElType.Arc =>  ArcPrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt),in.readFloat,in.readFloat)
		case PrintElType.Ellipse =>  EllipsePrintElement(readBounds(in),in.readFloat,in.readByte,new Color(in.readInt),in.readFloat,in.readFloat,in.readFloat)
		case PrintElType.Poly => new PolyPrintElement(in)
		case PrintElType.GraphText => GraphTextElement(readBounds(in),in.readUTF,in.readUTF,in.readInt,in.readFloat,in.readFloat,new Color(in.readInt),in.readFloat)
		case PrintElType.Filler => new FillPrintElement(readBounds(in),new Color(in.readInt),in.readInt,new Color(in.readInt)) 
		case PrintElType.DimLine =>			  
		  new DimLinePrintElement(readBounds(in),new Color(in.readInt),in.readInt,Expression.readConstant(in).toVector,
          in.readDouble,in.readDouble,DimensionPoint.readList(in),for(i<-0 until in.readInt) yield Expression.readConstant(in).toVector)		
		case PrintElType.ClipPrintElement=> new ClipPrintElement(readBounds(in))
		case PrintElType.ClipRestoreElement=> new RestoreClipPrintElement
		case PrintElType.Bitmap=> new BitmapPrintElement(readBounds(in),in.readUTF)
    case PrintElType.Symbol=> new SymbolPrintElement(readBounds(in),Reference(in),in.readDouble,in.readDouble,in.readUTF)
    case PrintElType.SymbolFiller=> new SymbolFillerPrintElement(readBounds(in),Reference(in),in.readDouble,in.readDouble,in.readUTF,
        VectorConstant.read(in),in.readInt,in.readDouble,in.readDouble)
    case PrintElType.DecoreMarker => DecoreMarkerElement
			case PrintElType.GraphBitmap => new GraphBitmapPrintElement(readBounds(in),in.readUTF,in.readDouble())
		case PrintElType.PageBreak => PageBreakMarker
		case other => throw new IllegalArgumentException("Unknown PrintElement-Type:"+other)
	}		
		
	def readBounds(in:DataInput)= new Rectangle2D.Float(in.readFloat,in.readFloat,in.readFloat,in.readFloat)
	
	val strokeMap=new FactoryMap[Float,BasicStroke](d=> {
		if (d==0.4f) new BasicStroke(0.3f, BasicStroke.CAP_SQUARE ,BasicStroke.JOIN_BEVEL , 1f, Array(3f,2f),0f) 
		else if (d==0.6f) new BasicStroke(0.3f, BasicStroke.CAP_SQUARE ,BasicStroke.JOIN_BEVEL , 1f, Array(1f,5f),0f) 
		else new BasicStroke(d)
		})	
}


class FactoryMap[A,B](factory:(A)=>B) extends collection.mutable.HashMap[A,B] {
	override def apply(a:A)= if (contains(a)) super.apply(a)
	else {
		val n=factory(a)
		update(a,n)
		n
	}
}