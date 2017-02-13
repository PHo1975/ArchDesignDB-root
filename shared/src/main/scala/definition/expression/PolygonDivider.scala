package definition.expression

import scala.util.control.NonFatal

abstract class PartArea(pl:PointList) {
  val pointList=if(pl.isClockWise)pl.reverse else pl
  val isSubtractArea= !pl.isClockWise
  def calcExpression:Expression
  def partLines:Seq[(VectorConstant,VectorConstant)]
  def texts:Seq[(VectorConstant,Double,String)]
  def checkSubstExpression(exp:Expression)= 
    if(isSubtractArea) new BinaryOperation(new DoubleConstant(-1d),BinOperator.getOp('*'),exp) else exp   
}

object NO_AREA_MATCH extends PartArea(new PointList(Seq())){
  val calcExpression=EMPTY_EX
  def partLines:Seq[(VectorConstant,VectorConstant)]=Seq.empty
  def texts:Seq[(VectorConstant,Double,String)]=Seq.empty
}


class TrianglePartArea(pl:PointList,dividerLines:Seq[(VectorConstant,VectorConstant)]=Nil) extends PartArea(pl) {  
  if(pointList.points.size!=3) throw new IllegalArgumentException("Triangle with "+pointList.points.size+" Points")  
  val maxEdgeLength=pointList.edgeLengths.zip(pointList.points.indices.iterator).maxBy{case (length,ix)=>length}
  val oppositePoint= pointList.points(pointList.getPrevPoint(maxEdgeLength._2))  
  val maxEdge=new Line3D(pointList.points(maxEdgeLength._2),pointList.points(pointList.nextVect(maxEdgeLength._2))
      -pointList.points(maxEdgeLength._2))
  def hitPoint=maxEdge.orthProjection(oppositePoint)  
  val partLines=(oppositePoint,hitPoint )+:dividerLines
  val perpendicularLength=(hitPoint-oppositePoint).toDouble
  val calcExpression=checkSubstExpression(TrianglePartArea.createTriangleExpression(maxEdgeLength._1,perpendicularLength))
  def firstTextPos=VectorConstant.midPoint(pointList.points(maxEdgeLength._2), hitPoint)+(oppositePoint-hitPoint)*0.02
  def secondTextPos=VectorConstant.midPoint(oppositePoint,hitPoint)+maxEdge.dir* -0.02
  val textAngle=pointList.edges(maxEdgeLength._2).XYAngle
  val texts=List((firstTextPos,textAngle,PolygonDivider.formatString.format(maxEdgeLength._1)),
      (secondTextPos,textAngle+PolygonDivider.PIHalf,PolygonDivider.formatString.format(perpendicularLength)))  
} 


class RectTrianglePartArea(pl:PointList,val partLines:Seq[(VectorConstant,VectorConstant)]=Seq.empty) extends PartArea(pl) {
   if(pointList.points.size!=3) throw new IllegalArgumentException("Triangle with "+pointList.points.size+" Points")
   
   val rightAngleIx=pointList.cosAngles.zip(pointList.points.indices.iterator)find{case (angle,ix)=> PolygonDivider.angleIsRight(angle,false)} match {
     case Some((angle,ix))=>ix
     case _=> throw new IllegalArgumentException("Rect Triangle with no rect agle")
   }
   val firstEdge=pointList.edges(pointList.getPrevPoint(rightAngleIx))
   val secondEdge=pointList.edges(rightAngleIx)
   val calcExpression=checkSubstExpression(TrianglePartArea.createTriangleExpression(firstEdge.length,secondEdge.length))
   def firstTextPos=PolygonDivider.textPointFromEdge(firstEdge,secondEdge)
   def secondTextPos=PolygonDivider.textPointFromEdge(secondEdge,firstEdge)  
   val textAngle=firstEdge.XYAngle
   val texts=List((firstTextPos,textAngle,PolygonDivider.formatString.format(firstEdge.length)),
       (secondTextPos,textAngle+PolygonDivider.PIHalf,PolygonDivider.formatString.format(secondEdge.length)))
}

class RectanglePartArea(pl:PointList,val partLines:Seq[(VectorConstant,VectorConstant)]=Seq.empty) extends PartArea(pl) {
   if(pointList.points.size!=4) throw new IllegalArgumentException("Rectangle with "+pointList.points.size+" Points")   
   val firstEdge=pointList.edges(0)
   val secondEdge=pointList.edges(1)
   val calcExpression=checkSubstExpression(PolygonDivider.createMultExpression(firstEdge.length,secondEdge.length))
   def firstTextPos=PolygonDivider.textPointFromEdge(firstEdge,secondEdge)
   def secondTextPos=PolygonDivider.textPointFromEdge(secondEdge,firstEdge)  
   val textAngle=firstEdge.XYAngle
   val texts=List((firstTextPos,textAngle,PolygonDivider.formatString.format(firstEdge.length)),
       (secondTextPos,textAngle+PolygonDivider.PIHalf,PolygonDivider.formatString.format(secondEdge.length)))   
}

abstract class AbstractTrapez(pl: PointList) extends PartArea(pl) {  
  if(pointList.points.size!=4) throw new IllegalArgumentException("Rectangle with "+pointList.points.size+" Points")     
  def firstEdge:Edge
  def secondEdge:Edge
  def thirdEdge:Edge
  def firstTextPos=PolygonDivider.textPointFromEdge(firstEdge,secondEdge)
  def secondTextPos=PolygonDivider.textPointFromEdge(secondEdge,firstEdge)  
  def thirdTextPos=PolygonDivider.textPointFromEdge(thirdEdge,secondEdge)   
  def textAngle=firstEdge.XYAngle
  lazy val texts=List((firstTextPos,textAngle,PolygonDivider.formatString.format(firstEdge.length)),
       (secondTextPos,textAngle+PolygonDivider.PIHalf,PolygonDivider.formatString.format(secondEdge.length)),
       (thirdTextPos,textAngle,PolygonDivider.formatString.format(thirdEdge.length)))
  lazy val calcExpression=checkSubstExpression(BinaryOperation(PolygonDivider.createDiv2Expression(PolygonDivider.createAddExpression(firstEdge.length,thirdEdge.length)),
       BinOperator.getOp('*'),DoubleConstant(PolygonDivider.roundValue(secondEdge.length))))
}

class RectTrapezPartArea(pl:PointList,rectAngles:Seq[Int],val partLines:Seq[(VectorConstant,VectorConstant)]=Seq.empty) extends AbstractTrapez(pl){    
   val firstEdge=pointList.edges(pointList.getPrevPoint(rectAngles(0)))
   val secondEdge=pointList.edges(rectAngles(0))
   val thirdEdge=pointList.edges(rectAngles(1))     
}

class TrapezPartArea(pl:PointList,parallelEdge:Int,val partLines:Seq[(VectorConstant,VectorConstant)]=Seq.empty) extends AbstractTrapez(pl) {   
   val firstEdge=pointList.edges(parallelEdge)
   val thirdEdge=pointList.edges(parallelEdge+2)
   val (perP1,perP2)= if(firstEdge.length<thirdEdge.length) {
     def hitPoint=thirdEdge.toLine3D.orthProjection(firstEdge.p1)
     if(!hitPoint.isInSegment(thirdEdge.p1,thirdEdge.p2)) (firstEdge.p2,hitPoint+firstEdge.diff)
     else(firstEdge.p1,hitPoint)       
   } else {
     def hitPoint=firstEdge.toLine3D.orthProjection(thirdEdge.p1)
     if(!hitPoint.isInSegment(firstEdge.p1,firstEdge.p2)) (thirdEdge.p2,hitPoint+thirdEdge.diff)
     else(thirdEdge.p1,hitPoint)
   }
   val secondEdge=new Edge(perP1,perP2)
   //println("trapezpartArea secondEdge:"+secondEdge)
}




object FourPointsPartArea{
  def switchRectParams(rectAngles:Seq[Int])= if(rectAngles(0)==0&&rectAngles(1)==3) Seq(3,0) else rectAngles
  
  def unapply(pointList:PointList):Option[PartArea]= {
    if(pointList.points.size==4) {
      val rectAngles=PolygonDivider.rectAnglesIndices(pointList)
      if(rectAngles.size==4) Some(new RectanglePartArea(pointList))
      else if(rectAngles.size==2){
        val swr=switchRectParams(rectAngles)
        if(swr(1)== pointList.nextVect(swr(0))) 
        Some(new RectTrapezPartArea(pointList,swr))
        else None
      }
      else if(pointList.edges(0).isParallelWith(pointList.edges(2))) Some(new TrapezPartArea(pointList,0))
      else if(pointList.edges(1).isParallelWith(pointList.edges(3))) Some(new TrapezPartArea(pointList,1))
      else None
    }
    else None
  }
}


object TrianglePartArea {  
  def createTriangleExpression(a:Double,b:Double)= PolygonDivider.createDiv2Expression(PolygonDivider.createMultExpression(a,b))      
  
  def unapply(pointList:PointList)= {
    if(pointList.points.size==3){      
      if(PolygonDivider.hasRectAngle(pointList,pointList.isClockWise)) Some(new RectTrianglePartArea(pointList)) 
      else Some(new TrianglePartArea(pointList))
    }
    else None
  }  
}

object RectPart {  
  def unapply(pl:PointList):Option[(PartArea,Seq[PointList])]= if(pl.points.size>=4) { 
     def pointList=pl
     val fullAngles= Polygon.ringLoop(pointList.cosAngles.zip(pointList.points.indices.iterator).toSeq).toList      
     PolygonDivider.foreachPair[(Double,Int)] (fullAngles, (el:(Double,Int)) => {PolygonDivider.angleIsRight(el._1,pl.isClockWise)},(first,second)=>{
       val firstIx=first._2
       val secondIx=second._2         
       val firstEdge=pointList.edges(pointList.getPrevPoint(firstIx))
       val thirdEdge=pointList.edges(secondIx)
       val deltaVect=if(thirdEdge.length<firstEdge.length) thirdEdge.diff 
       	 else firstEdge.p1-firstEdge.p2       
       val thisArea=new Polygon(Nil,Seq(pointList))
       val testRect=new PointList(Seq( firstEdge.p2,thirdEdge.p1,thirdEdge.p1+deltaVect, firstEdge.p2+deltaVect))       
       val testArea=new Polygon(Nil,Seq(testRect))
       val allowedPoints=Seq(firstEdge.p1,firstEdge.p2,thirdEdge.p1,thirdEdge.p2)
       val otherPointsInRect=pointList.points.filter(p=> !allowedPoints.contains(p)&&testArea.contains(p))
       val result=if(otherPointsInRect.isEmpty)Some((testArea,deltaVect)) else {
         val line=pointList.edges(firstIx).toLine3D
         val minDistance= otherPointsInRect.map(apoint=>line.distanceTo(apoint)*(if(line.pointLocation2D(apoint)<0) -1 else 1)).min
         if(minDistance<=0)None
         else {
           val newDeltaVect=deltaVect.unit*minDistance
           Some((new Polygon(Nil,Seq(new PointList(Seq( firstEdge.p2,thirdEdge.p1,thirdEdge.p1+newDeltaVect, firstEdge.p2+newDeltaVect)))),newDeltaVect))
         }
       }
       for((cutArea,deltVect)<-result;if(StrictMath.abs(cutArea.getAreaValue)>Polygon.treshold)) {
           val restArea=thisArea.subtract(cutArea)
           val restList=if(!pointList.isClockWise) restArea.pathList.map(_.reverse) else restArea.pathList
		       return Some((new RectanglePartArea(cutArea.pathList.head,Seq((pointList.points(secondIx)+deltVect,
		           pointList.points(firstIx)+deltVect))),restList))
        }         
	   }) 
     None          
  } else None
}

object TrapezPart {
  protected def createRestList(pointList:PointList,shape:PointList,origIsClockWise:Boolean)= {
    val thisArea=new Polygon(Nil,Seq(pointList)) 
    val cutArea=new Polygon(Nil,Seq(shape))
    val restArea=thisArea.subtract(cutArea)
    if(!origIsClockWise) restArea.pathList.map(_.reverse) else restArea.pathList
  }
  
  def unapply(pl:PointList):Option[(PartArea,Seq[PointList])]= if(pl.points.size>=4){
    val pointList=if(pl.isClockWise)pl.reverse else pl
    val angles=pointList.cosAngles.toSeq
    def zippedAngles=Polygon.ringLoop(angles.zip(pointList.points.indices)).sliding(3)
    // check for Trapezes with one rect angle
    //println("Angles:")
    //println(angles.zip((pointList.points.indices)).mkString("| "))
    zippedAngles. foreach{
      case Seq((firstAngle,fix),(middleAngle,mix),(lastAngle,lix)) => 
        if (PolygonDivider.angleIsRight(middleAngle,false)&& (firstAngle>0 || lastAngle>0)){
          //println("found  fix:"+fix+" mix:"+mix+" lix:"+lix+ " firstangle:"+firstAngle+" ma:"+middleAngle)
      
	        val (testTrap,line,delta)=if(firstAngle>0) {
	          val firstEdge=pointList.edges(pointList.getPrevPoint(fix))
	          val thirdEdge=pointList.edges(mix)
	          val line=pointList.edges(fix).toLine3D
	          val dist=StrictMath.min(line.distanceTo(firstEdge.p1),line.distanceTo(thirdEdge.p2))
	          val deltaVect=thirdEdge.diff.unit*dist
	          //println("dist:"+dist+" delta:"+deltaVect)
	          val startPoint=new Line3D(line.pos+deltaVect,line.dir).intersectionWith(firstEdge.toLine3D)
	          (new PointList(Seq(firstEdge.p2,thirdEdge.p1,thirdEdge.p1+deltaVect,startPoint)),line,thirdEdge.diff.unit)         
	        } else {
	          val firstEdge=pointList.edges(fix)
	          val thirdEdge=pointList.edges(lix)
	          val line=pointList.edges(mix).toLine3D
	          val dist=StrictMath.min(line.distanceTo(firstEdge.p1),line.distanceTo(thirdEdge.p2))
	          val deltaVect=firstEdge.diff.unit*(-dist)
	          //println("dist:"+dist+" delta:"+deltaVect)
	          val startPoint=new Line3D(line.pos+deltaVect,line.dir).intersectionWith(thirdEdge.toLine3D)
	          (new PointList(Seq(firstEdge.p2,thirdEdge.p1,startPoint,firstEdge.p2+deltaVect)),line,firstEdge.diff.unit.revert)
	        }
	        //println ("TestTrap:"+testTrap+" line:"+line+" delta:"+delta)
	        if(StrictMath.abs(testTrap.getArea)>Polygon.treshold) {
		        val testArea=new Polygon(Nil,Seq(testTrap))
		        
		        val otherPointsInTrap = pointList.points.filter(p=> !testTrap.points.contains(p)&&testArea.contains(p)) 
		        //println("otherpointsinTrap:"+otherPointsInTrap.mkString("| "))
		        val result = if(otherPointsInTrap.isEmpty) Some(testTrap)  
		        else {
		          val minDistance= otherPointsInTrap.map(apoint=>line.distanceTo(apoint)*(if(line.pointLocation2D(apoint)<0) -1 else 1)).min
		         if(minDistance<=0){util.Log.e("points behind Trapez "+minDistance);None}
		         else {
		           val newDeltaVect=delta*minDistance
		           //println("Min distance:"+minDistance)
		           val subLine=new Line3D(line.pos+newDeltaVect,line.dir)
		           val pu1=subLine.intersectionWith(new Line3D(testTrap.points(1),testTrap.points(2)-testTrap.points(1)))
		           val pu2=subLine.intersectionWith(new Line3D(testTrap.points(0),testTrap.points(3)-testTrap.points(0)))
		           Some(new PointList(Seq( testTrap.points(0),testTrap.points(1),pu1, pu2)))
		         }          
		        }
		        for(cutTrap<-result)
		          return Some((new RectTrapezPartArea(if(pl.isClockWise) cutTrap.reverse else cutTrap,if(firstAngle>0)Seq(1,2) else Seq(3,0),
					          Seq((cutTrap.points(2),cutTrap.points(3)))),createRestList(pointList,cutTrap,pl.isClockWise)))	        
	        }
	      }  
    }
    // check for trapezes with 2 parallel edges
    for(ix<- pointList.points.indices; if (pointList.edges(pointList.getPrevPoint(ix)).
        isParallelWith(pointList.edges(pointList.nextVect(ix)))&&
      angles(ix)>0 && angles(pointList.nextVect(ix))>0 )) { 
      //println("2 parallels "+ix+" firstEdge:"+pointList.getPrevPoint(ix))
      val firstEdge= pointList.edges(pointList.getPrevPoint(ix))
      val thirdEdge= pointList.edges(pointList.nextVect(ix))
      val cutRect=new PointList(Seq(firstEdge.p2,thirdEdge.p1,thirdEdge.p2,firstEdge.p1))      
      if(StrictMath.abs(cutRect.getArea)>Polygon.treshold){ 
	      val testArea=new Polygon(Nil,Seq(cutRect))
	      val otherPointsInTrap = pointList.points.filter(p=> !cutRect.points.contains(p)&&testArea.contains(p))	      
	      val result = if(otherPointsInTrap.isEmpty) Some(cutRect)  
	      else {      
	        val (line,delta)={
	            val firstCross=firstEdge.toLine3D.orthProjection(thirdEdge.p1)	            
	            if(firstCross.isInSegment(firstEdge.p1,firstEdge.p2)) (new Edge(firstCross,thirdEdge.p1),firstCross-firstEdge.p1)
	            else {
	              val secondCross=thirdEdge.toLine3D.orthProjection(firstEdge.p2)
	              (new Edge(firstEdge.p2,secondCross),secondCross-thirdEdge.p1)	            
	            }
	        }	        
	        val minDistance= otherPointsInTrap.map(apoint=>line.toLine3D.distanceTo(apoint)*(if(line.pointLocation2D(apoint)<0) -1 else 1)).min
	        if(minDistance<=0){util.Log.e("points behind ParTrapez "+minDistance);None}
	        else {	          
	          val newDeltaVect=delta.unit*minDistance	          	          
	          Some(new PointList(Seq( firstEdge.p2, thirdEdge.p1, line.p2+newDeltaVect, line.p1+newDeltaVect)))
	        }          
	      }
	      for(cutTrap<-result)         
	          return Some((new TrapezPartArea(if(pl.isClockWise) cutTrap.reverse else cutTrap,1,
	              Seq((cutTrap.points(2),cutTrap.points(3)))),createRestList(pointList,cutTrap,pl.isClockWise)))      
      } //else println("are too small")  
    } 
    
    var foundTrapsWithOtherPoints:Option[(PointList,Line3D,Seq[VectorConstant],VectorConstant)]=None
    // cut trapezes that do not have inner points
    for(ix<-pointList.points.indices;if(angles(ix)>0&&(angles(pointList.nextVect(ix))>0))) {
       val firstEdge=pointList.edges(pointList.getPrevPoint(ix))
       val thirdEdge=pointList.edges(pointList.nextVect(ix))
       val line=pointList.edges(ix).toLine3D
       val firstDistance=line.distanceTo(firstEdge.p1)
       val secondDistance=line.distanceTo(thirdEdge.p2)
       val delta =if(firstDistance>secondDistance) {
         line.orthogonalThrough(thirdEdge.p2)
       } else {
         line.orthogonalThrough(firstEdge.p1)
       }
       val testCutLine=new Line3D(line.pos+delta,line.dir)
       val p1=testCutLine.intersectionWith(firstEdge.toLine3D)
       val p2=testCutLine.intersectionWith(thirdEdge.toLine3D)
       val testCutTrap=new PointList(Seq(firstEdge.p2,thirdEdge.p1,p2,p1))
       val testArea=new Polygon(Nil,Seq(testCutTrap))
       val otherPointsInTrap = pointList.points.filter(p=> !testCutTrap.points.contains(p)&&testArea.contains(p))
       if(otherPointsInTrap.isEmpty) {
         return Some((new TrapezPartArea(if(pl.isClockWise) testCutTrap.reverse else testCutTrap,0,
	              Seq((testCutTrap.points(2),testCutTrap.points(3)))),createRestList(pointList,testCutTrap,pl.isClockWise)))
       } else {
         if(!foundTrapsWithOtherPoints.isDefined) foundTrapsWithOtherPoints=Some((testCutTrap,line,otherPointsInTrap,delta))          
       }
    }
    // cut trapezes with inner point, if found
    for((trap,line,otherPoints,delta)<-foundTrapsWithOtherPoints){
      //println("Special case ")
      val minDistance= otherPoints.map(apoint=>line.distanceTo(apoint)*(if(line.pointLocation2D(apoint)<0) -1 else 1)).min
		  if(minDistance>0) {		    	          
		    val newDeltaVect=delta.unit*minDistance	          	          
		    val newTrap=new PointList(Seq( trap.points(0),trap.points(1), line.pos+line.dir+newDeltaVect, line.pos+newDeltaVect))       
        return Some((new TrapezPartArea(if(pl.isClockWise) newTrap.reverse else newTrap,1,
	              Seq((line.pos+line.dir+newDeltaVect,line.pos+newDeltaVect))),createRestList(pointList,newTrap,pl.isClockWise)))  
      }
    }
    // find triangles
    zippedAngles. foreach{
      case Seq((firstAngle,fix),(middleAngle,mix),(lastAngle,lix)) => 
        if (middleAngle>0&& firstAngle<0 && lastAngle<0){
          val firstEdge=pointList.edges(fix)
          val testTri=new PointList(Seq(firstEdge.p1,firstEdge.p2,pointList.points(lix)))
          val testArea=new Polygon(Nil,Seq(testTri))
          if(!pointList.points.exists(p=> !testTri.points.contains(testTri)&& testArea.contains(p))) {
            
            return Some((new TrianglePartArea(if(pl.isClockWise) testTri.reverse else testTri,Seq((firstEdge.p1,pointList.points(lix)))),
                createRestList(pointList,testTri,pl.isClockWise)))
          }
        }
    }
    None    
  } else None  
}



object PolygonDivider {
  val PIHalf=StrictMath.PI/2d
  val formatString="%,.3f"
    
  /*@annotation.tailrec def findFirstPair[T](list:List[T],func:(T)=>Boolean):Option[(T,T)]= list match {
    case Nil=> None
    case elem::Nil=>None
    case first::second=> if(func(first) && func(second.head)) return Some((first,second.head))
     else findFirstPair(list.tail,func)
  } */
  
  @annotation.tailrec def foreachPair[T](list:List[T],checkFunc:(T)=>Boolean,execFunc:(T,T)=>Unit):Unit= list match {
    case Nil=> 
    case elem::Nil=>
    case first::second=> {
      if(checkFunc(first) && checkFunc(second.head)) execFunc(first,second.head)
      foreachPair(list.tail,checkFunc,execFunc)
    }    
  }
  
   
  
  def rectAnglesIndices(pointList:PointList)=pointList.cosAngles.zip(pointList.points.indices.iterator).filter{
    case(angle,ix)=>PolygonDivider.angleIsRight(angle,pointList.isClockWise)}.map(_._2).toList  
    
  def createMultExpression(a:Double,b:Double)=BinaryOperation(DoubleConstant(PolygonDivider.roundValue(a)),
      BinOperator.getOp('*'), DoubleConstant(PolygonDivider.roundValue(b)))
      
  def createAddExpression(a:Double,b:Double)=BinaryOperation(DoubleConstant(PolygonDivider.roundValue(a)),
      BinOperator.getOp('+'), DoubleConstant(PolygonDivider.roundValue(b)))
      
  def createDiv2Expression(a:Expression)=BinaryOperation(a,BinOperator.getOp('/'), DoubleConstant(2d))    
    
  def angleIsRight(angle:Double,clockWise:Boolean)=if(clockWise) StrictMath.abs(angle+PolygonDivider.PIHalf)<Polygon.treshold else StrictMath.abs(angle-PolygonDivider.PIHalf)<Polygon.treshold
  def hasRectAngle(pointList:PointList,clockWise:Boolean=true)=pointList.cosAngles.exists(angleIsRight(_,clockWise))
  
  //def getAngle (p1:VectorConstant,p2:VectorConstant,p3:VectorConstant)= (p2-p1).angleBetween(p3-p2)
  
  def roundValue(value:Double)=StrictMath.round(value*1000d)/1000d

  protected def _divideArea(pointList:PointList):Seq[PartArea]= try {
    if(StrictMath.abs(pointList.getArea)<Polygon.treshold) Seq.empty
    //println("Divide Area pointList.size:"+pointList.points.size+" clockwise:"+pointList.isClockWise)
    else pointList match {
      case FourPointsPartArea(fourPoints)=>List(fourPoints)
      case TrianglePartArea(triangle)=>List(triangle)      
      case restList =>{
        if(StrictMath.abs(restList.getArea)<Polygon.treshold) Seq.empty
        else {
          val (foundElem, arestList) = pointList match {
            case RectPart(rectangle, restList) => (rectangle, restList)
            case TrapezPart(trapez, restList) => (trapez, restList)
            case rest => util.Log.e("No match " + rest.points.mkString("|")); return Seq(NO_AREA_MATCH)
          }
          foundElem +: arestList.flatMap(pList => _divideArea(pList.removeDoublePoints .removeStraightEdges))
        }
      } 
  	}
  } catch {case NonFatal(e)=> println(e.toString+" "+e.getStackTrace().take(20).mkString("\n"));Seq.empty}
  
  def divideArea(pointList:PointList):Seq[PartArea]= {
    _divideArea(pointList).filter(pl=>StrictMath.abs(pl.pointList.getArea)>Polygon.treshold)
  }
  
  def combineExpression(areaList:Seq[PartArea])=if(areaList.contains(NO_AREA_MATCH)) EMPTY_EX 
  		else areaList.map(_.calcExpression).reduceLeft(BinaryOperation(_,BinOperator.getOp('+'),_))
  
  def textPointFromEdge(edge:Edge,rectEdge:Edge)= VectorConstant.midPoint(edge.p1, edge.p2)+rectEdge.diff*0.02
}