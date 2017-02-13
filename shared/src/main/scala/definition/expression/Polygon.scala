package definition.expression

import java.awt.geom.Area
import java.awt.geom.Path2D
import java.awt.geom.PathIterator
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import java.io.DataInput
import java.io.DataOutput

import scala.Array.fallbackCanBuildFrom

import definition.data.Referencable
import definition.data.Reference
import definition.typ.DataType


//case class InterPoint(nx:Double,ny:Double) extends VectorConstant(nx,ny,0)

class Edge(val p1:VectorConstant,val p2:VectorConstant) {
  lazy val minX=scala.math.min(p1.x,p2.x)
  lazy val maxX=scala.math.max(p1.x,p2.x)
  lazy val minY=scala.math.min(p1.y,p2.y)
  lazy val maxY=scala.math.max(p1.y,p2.y)  
  
  def dx=p2.x-p1.x
  def dy=p2.y-p1.y
  
  def diff=p2-p1
  
  def toLine3D=new Line3D(p1,p2-p1)
  
  def XYAngle= math.atan2(dy,dx)
  
  def pointLocation2D(point:VectorConstant):Double= {
	  (p2.x-p1.x)*(point.y-p1.y)-(point.x-p1.x)*(p2.y-p1.y)
	}
  
  def isParallelWith(other:Edge)= StrictMath.abs(VectorConstant.det2D(other.dx,other.dy,dx,dy))<Polygon.treshold
  
  def length=math.sqrt(dx*dx+dy*dy)
  
  /** intersection with ray
   * @return List of hitpoint if existent, as (positionOnOtherLine,HitPoint)
   * hitpoint is in other line when positionOnOtherLine >=0 & <=1
   */
  def getIntersectionWith(op1:VectorConstant,op2:VectorConstant):Seq[(Double,VectorConstant)]= {    
  	val ody=op2.y-op1.y
  	val odx=op2.x-op1.x
  	val d=ody*dx-odx*dy
  	if(d!=0) {
  		val ua=(odx*(p1.y-op1.y)-ody*(p1.x-op1.x))/d
  		val ub=(dx*(p1.y-op1.y)-dy*(p1.x-op1.x))/d
  		//println("UA = "+ua)
  		if((ua==0 &&VectorConstant.pointLocation2D(op1,op2,p2)>0) ||
  		(ua==1 && VectorConstant.pointLocation2D(op1,op2,p1)>0))  Seq.empty
  		else if(ua>=0 && ua<=1){
  			val x=p1.x+ua*dx
  			val y=p1.y+ua*dy
  			List((ub,new VectorConstant(x,y,0)))
  		} else Seq.empty
  	}     	
    else Seq.empty
  }
  
  def getCutIntersectionWith(op1:VectorConstant,op2:VectorConstant):Seq[(Double,VectorConstant)]= {
    val cutTreshold=0.000001
  	val ody=op2.y-op1.y
  	val odx=op2.x-op1.x
  	val d=ody*dx-odx*dy
  	if(d!=0) {
  		val ua=(odx*(p1.y-op1.y)-ody*(p1.x-op1.x))/d
  		val ub=(dx*(p1.y-op1.y)-dy*(p1.x-op1.x))/d  		  		 
  		if(ua>= -cutTreshold && ua<=1+cutTreshold){
  			val x=p1.x+ua*dx
  			val y=p1.y+ua*dy
  			List((ub,new VectorConstant(x,y,0)))
  		} else Seq.empty
  	}     	
    else Seq.empty
  }
  
  
  /** intersection with other edge
   * 
   */
  def getIntersectionWith(other:Edge):Option[VectorConstant]= 
    if(other.minX>maxX||other.maxX<minX||other.minY>maxY||other.maxY<minY/*||isLinearyDependendFrom(other)*/) None 
    else {    
      val d=other.dy*dx-other.dx*dy
      if(d==0) None // parallel
      else {
        val ua = (other.dx * (p1.y - other.p1.y) - other.dy * (p1.x - other.p1.x)) / d
        val ub = (dx * (p1.y - other.p1.y) - dy * (p1.x - other.p1.x)) / d
        if ((ua == 0 && (ub == 0 || ub == 1)) || (ua == 1 && (ub == 0 || ub == 1))) None // both are endpoints
        else if (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1) {
          // inside the segments
          val x = p1.x + ua * dx
          val y = p1.y + ua * dy
          Some(new VectorConstant(x, y, 0))
        }
        else None
      }
    }
  
   
  override def toString="Edge "+p1+" - "+p2+" "
  
}

case class PointList(points:Seq[VectorConstant]) {
  
  def createPath2D= {
    val path=new Path2D.Double
    if(points.size>2){
    	path.moveTo(points.head.x,points.head.y)
    	for(i<- 1 until points.size;p=points(i))
    	  path.lineTo(p.x,p.y)
    	path.closePath()
    }	
    path
  }  
  
  lazy val path2D=createPath2D
  def numVecs=points.size
  
  def nextVect(i:Int)=if(i<numVecs-1)i+1 else 0
  
  def getPrevPoint(point:Int)=if(point-1>=0) point-1 else points.size-1
  
  lazy val edges:Seq[Edge]= points.indices.map(i=> new Edge(points(i),points(nextVect(i))))
  lazy val minX=if(points.isEmpty)0 else points.reduceLeft((a,b)=>if(a.x<b.x)a else b).x
  lazy val minY=if(points.isEmpty)0 else points.reduceLeft((a,b)=>if(a.y<b.y)a else b).y
  lazy val maxX=if(points.isEmpty)0 else points.reduceLeft((a,b)=>if(a.x>b.x)a else b).x
  lazy val maxY=if(points.isEmpty)0 else points.reduceLeft((a,b)=>if(a.y>b.y)a else b).y
  
  lazy val getArea= {
    if(numVecs<3) 0
    else (points.head.x*(points(1).y-points(numVecs-1).y)+points(numVecs-1).x*(points.head.y-points(numVecs-2).y)+
    (1 until numVecs-1).foldLeft(0d)((r,ix)=> r+points(ix).x*(points(ix+1).y-points(ix-1).y)) )/2
  }
  
  def getMidPoint= if(numVecs<3) mid else {
    val area=getArea
    val sp=points(numVecs-1)
    val snp=points.head
    val sfact= sp.x * snp.y - snp.x * sp.y
    var rx=(sp.x+snp.x)*sfact
    var ry=(sp.y+snp.y)*sfact
    for(i<-0 until numVecs-1){
      val p=points(i)
      val np=points(i+1)
      val fact= p.x * np.y - np.x * p.y
      rx+=(p.x+np.x)*fact
      ry+=(p.y+np.y)*fact
    }
    val fact2=6d*area
    new VectorConstant(rx/fact2,ry/fact2,0)
    
  }
  
  def getUmfang= if(numVecs<3) 0d else
    (1 until numVecs).foldLeft(0d)((s,ix)=> s+(points(ix)-points(ix-1)).toDouble)+(points.head-points(numVecs-1)).toDouble
  
  private def mid=if(points.size==0)NULLVECTOR else new VectorConstant(points.foldLeft(0d)(_ + _.x)/points.size,
      points.foldLeft(0d)(_ + _.y)/points.size,points.foldLeft(0d)(_ + _.z)/points.size)
  
  lazy val isClockWise=getArea<0
  
  lazy val centerPoint:VectorConstant=if(points.size==0) NULLVECTOR else
    points.foldLeft[VectorConstant](NULLVECTOR)(_ + _)*(1/points.size)
  
  def minEdgeDistanceToPoint(point:VectorConstant)=(edges map(e=>scala.math.abs(e.pointLocation2D(point)))).min
    
  def translate(v:VectorConstant)= new PointList(points.map(_ +v))
  
  def translatePoints(hitPoints:Set[VectorConstant],delta:VectorConstant)= new PointList(points.map(p=> if(hitPoints.contains(p)) p+delta else p))
  def transform(trans:(VectorConstant)=>VectorConstant) = new PointList( points.map(trans))
  
  def pointsOutsideFromEdge(edge:Edge)= {
    points.exists(edge.pointLocation2D(_)>0)
  }
  
  def reverse=new PointList(points.reverse)
  
  def clockWise=if(getArea>0) reverse else this
  
  override def toString="P("+points.mkString(";")+")"
  
  def write(file: DataOutput): Unit = {
    file.writeInt(points.size)
    points.foreach(_.write(file))
  }
  
  def removeStraightEdges()= {
    val straightEdges=angles.zip(points.indices.iterator).filter{case (angle,ix)=> StrictMath.abs(StrictMath.abs(angle)-1d)<Polygon.treshold}.toSeq
    if(straightEdges.isEmpty) this
    else new PointList(points.zipWithIndex.filterNot{case (point,pix)=> straightEdges.exists{case (angle,aix)=>aix==pix}}.map(_._1))
  }
  
  def removeDoublePoints()= {
    val doublePoints= points.indices.filter(i=> VectorConstant.similar(points(i), points(nextVect(i))))
    if(doublePoints.isEmpty) this
    else new PointList(points.zipWithIndex.filterNot{case (point,pix)=> doublePoints.contains(pix)}.map(_._1))
  }
  
  def encode(): String = points.map(_.encode) .mkString("§")
  
  def angles=Polygon.ringLoop(points).sliding(3).map{case List(p1,p2,p3)=>
    VectorConstant.getAngle(p1,p2,p3)
  }
  
  def cosAngles=Polygon.ringLoop(points).sliding(3).map { case List(p1,p2,p3)=>
    StrictMath.acos(VectorConstant.getAngle(p1,p2,p3))-(if(VectorConstant.pointLocation2D(p1,p2,p3)<0)StrictMath.PI else 0)
  }
  
  def edgeLengths=points.indices.iterator.map(ix=> (points(nextVect(ix))-points(ix)).toDouble)
}


class Polygon (val parents:Seq[Referencable],val pathList:Seq[PointList]=Seq.empty) extends Constant {

  def toInt: Int =  toDouble.toInt 
  def toLong: Long =  toDouble.toLong
  def toDouble: Double =  pathList.size
  def toBoolean: Boolean =  pathList.size>0
  def getNative: Any =  this 
  def getType: DataType.Value =  DataType.PolygonTyp 
  def createCopy(): Expression = { new Polygon(parents,pathList) }
  def getTerm: String =  "Pa["+pathList.mkString("|")+"]" 
  override def toString=getTerm
  
  private lazy val testPoint=new Point2D.Double
  
  lazy val minX=if(pathList.isEmpty)0 else pathList.reduceLeft((a,b)=>if(a.minX<b.minX)a else b).minX
  lazy val minY=if(pathList.isEmpty)0 else pathList.reduceLeft((a,b)=>if(a.minY<b.minY)a else b).minY
  lazy val maxX=if(pathList.isEmpty)0 else pathList.reduceLeft((a,b)=>if(a.maxX>b.maxX)a else b).maxX
  lazy val maxY=if(pathList.isEmpty)0 else pathList.reduceLeft((a,b)=>if(a.maxY>b.maxY)a else b).maxY
  
  def getBounds= new Rectangle2D.Float(minX.toFloat,minY.toFloat,(maxX-minX).toFloat,(maxY-minY).toFloat)
  
  lazy val getAreaValue = -1d*pathList.foldLeft(0d)((sum,value)=>{sum + value.getArea})
  
  import Polygon.{treshold,contTreshold,areaTreshold}
  
  def isEmpty=scala.math.abs(getAreaValue)<areaTreshold
  
  def toPath:Path2D.Double= {
    val pa=new Path2D.Double
    if(pathList.nonEmpty){
      for(pList<-pathList;if pList.points.size > 2) {
        pa.moveTo(pList.points.head.x,pList.points.head.y)
        for(ix<-1 until pList.points.size)
          pa.lineTo(pList.points(ix).x,pList.points(ix).y)
        pa.closePath()
      }      
    }
    pa
  }
  
  def toPathTransformed(trans:(VectorConstant)=>VectorConstant):Path2D.Double= {
    val pa=new Path2D.Double
    if(pathList.nonEmpty){
      for(pList<-pathList;if pList.points.size > 2) {
        val transf=trans(pList.points.head)
        pa.moveTo(transf.x,transf.y)
        for(ix<-1 until pList.points.size;p=trans(pList.points(ix)))
          pa.lineTo(p.x,p.y)
        pa.closePath()
      }      
    }
    pa
  }
  
  def transform(trans:(VectorConstant)=>VectorConstant) = new Polygon(parents,pathList.map(_.transform(trans)))
  
  
  /** finds the minimal and maximal distances to the point (0,0) of this polygon
   * 
   */
  def findMinMaxHatchDistances(dir:VectorConstant,norm:VectorConstant,startPoint:VectorConstant):(Double,Double)={
    val du=dir.unit
    val dir2=du*du
    //val norm=new VectorConstant(-du.y,du.x,0)
    var minDist=Double.MaxValue
    var maxDist=Double.MinValue
    for(pl<-pathList;p<-pl.points){
      val hdist=p.hatchDistance(du,dir2,norm,startPoint)
      if(hdist<minDist) minDist=hdist
      if(hdist>maxDist) maxDist=hdist
    }
    (minDist,maxDist)
  }
  
  lazy val path=toPath
  
  lazy val area=new Area(path)
  
  def areaClone=area.clone.asInstanceOf[Area]
   
  
  def write(file: DataOutput): Unit = { 
    //System.out.println("write poly ")
    file.writeByte(DataType.PolygonTyp.id)
    file.writeInt(parents.size)
    parents.foreach(_.ref.write(file))
    file.writeInt(pathList.size)
    pathList.foreach(_.removeDoublePoints(). /*counterClockWise.*/ removeStraightEdges().write(file))
  }
  def encode: String = "$A["+pathList.map(_.encode()).mkString("|")+"]"
   
  
  override def toPolygon=this
  
  def translate(v:VectorConstant)= new Polygon(parents,pathList.map(_.translate(v)))
  
  /** translates only points of this polygon that are part of the given points Sequence
   * 
   */
  def translatePoints(points:Set[VectorConstant],delta:VectorConstant)= new Polygon(parents,pathList.map(_.translatePoints(points,delta)))
  
  def intersectionsWith(p1:VectorConstant,p2:VectorConstant)= 
    pathList.flatMap(_.edges.flatMap(_.getIntersectionWith(p1,p2))).sortBy(a=>a._1)//.distinct
  
    
  
  def contains(v:VectorConstant) = {
    testPoint.x=v.x
    testPoint.y=v.y
    if(area.contains(testPoint)) {
      val med=minEdgeDistanceToPoint(v)
      //println("check contains "+med)
      med>contTreshold  
    } else false
  }
  
  def minEdgeDistanceToPoint(point:VectorConstant)=(pathList map(_.minEdgeDistanceToPoint(point))).min
  
  def intersectsWith(other:Polygon)= {    
    if(other.maxX-treshold<=minX||other.minX+treshold>=maxX||other.maxY-treshold<=minY||other.minY+treshold>=maxY) false
    else {
      scala.math.abs(intersect(other).getAreaValue)>treshold
    }
    //else other.pathList.exists(_.points.exists(contains(_)))|| pathList.exists(_.points.exists(other.contains(_)))
  }  
  
  
  def intersect(other:Polygon):Polygon={
    val cl=areaClone
    cl.intersect(other.area)
    new Polygon(parents++other.parents,Polygon.areaToPoints(cl))
    //setArea(cl)
  }
  
  def subtract(other:Polygon):Polygon= {
    val cl=areaClone
    cl.subtract(other.area)
    //setArea(cl)
    new Polygon(parents++other.parents,Polygon.areaToPoints(cl))
  }
  
  def add(other:Polygon):Polygon= {
    val cl=areaClone
    cl.add(other.area)
    //setArea(cl)
    new Polygon(parents++other.parents,Polygon.areaToPoints(cl))
  }
  
  def setArea(newArea:Area):Polygon= new Polygon(parents,Polygon.areaToPoints(newArea))
  
  
}

object Polygon {
  val treshold=0.000001d
  val contTreshold=0.00000000000001d
  val areaTreshold =0.0000001d
  def decode(text:String)= {
	  val end=text.indexOf(']',3)
	  val parts=text.substring(3,end).split('|')
	  (new Polygon(Seq.empty,parts.map(decodePointList)),end+1)
  }
  
  def decodePointList(text:String):PointList= new PointList(text.split('§').map(VectorConstant.decode(_)._1))
  
  private [expression] def apply(file:DataInput)= {
    new Polygon(for(i<-0 until file.readInt) yield Reference(file),for(i<-0 until file.readInt) yield readPointList(file) )
  }
  
  def readPointList(file:DataInput)= new PointList( for(i<-0 until file.readInt) yield{
        file.readByte
        new VectorConstant(file.readDouble,file.readDouble,file.readDouble)})
  
  def areaToPoints(area:Area):Seq[PointList]= {
    val iter=area.getPathIterator(null)
    //print("wind :"+iter.getWindingRule()==PathIterator.WIND_NON_ZERO)
    val retArray=new Array[Double](6)
    val pathList=new collection.mutable.ArrayBuffer[PointList]()
    var pList:collection.mutable.ArrayBuffer[VectorConstant]=null
    while(!iter.isDone) {      
      iter.currentSegment(retArray) match {
        case PathIterator.SEG_MOVETO=> pList=collection.mutable.ArrayBuffer[VectorConstant](new VectorConstant(retArray(0),retArray(1),0))
        case PathIterator.SEG_LINETO=> pList+= new VectorConstant(retArray(0),retArray(1),0)
        case PathIterator.SEG_CLOSE => pathList+=new PointList(pList)        
      }   
      iter.next()
    }
    pathList
  }
  
  def ringLoop[A](s:Iterable[A])= {    
    new Iterator[A]{
      val intIter=s.iterator
      var state=0
      def hasNext= state match {
        case 0=> s.nonEmpty
        case 1=> true        
        case _=> false
      }
      def next()= state match {
        case 0=> state=1;s.last
        case 1=> if(intIter.hasNext)intIter.next() else {
          state=2;s.head
        }
        case _=> throw new IllegalArgumentException("Read after end of Ring")
      }
    }
  }
  
  def midOfPointList(pointLists:Seq[PointList])=
    if(pointLists.isEmpty) NULLVECTOR
    else {
      val sum=pointLists.foldLeft((0d,0d,0d,0d))((i,v)=>{
        val area= -1d*v.getArea
	      val vm=v.getMidPoint
	      (i._1+vm.x*area,i._2+vm.y*area,i._3+vm.z*area,i._4+area)
	    })
	    val size=sum._4
	    new VectorConstant(sum._1/size,sum._2/size,sum._3/size)
    } 
  
  
}

object NULLPOLYGON extends Polygon(Seq.empty)