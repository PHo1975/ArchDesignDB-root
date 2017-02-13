/**
 * Author: Peter Started:13.05.2011
 */
package definition.expression

//import java.io._





/** a plane in3D Space
 * @param pos the Position of the plane in 3D Space
 * @param dir the norm vector for the direction of the plane 
 */
class Plane3D(val pos:VectorConstant,val dir:VectorConstant) {
  lazy val dirUnit=dir.unit
	
  //import Plane3D._ 
  
  
  //implicit def builder[A <:Plane3D]:PlaneBuilder[A]=SimpleBuilder
  
	def isLinearyDependentFrom(other:Plane3D)= dir.isLinearyDependentFrom(other.dir)
	
	def isLinearyDependentFrom(line:Line3D)= (dir*line.dir) == 0d
	
	def isVertical= dir.z==0d
	
	def isHorizontal = dir.x==0d && dir.y==0d
	
	def isDefined= !dir.isNull
	
	def getDistanceTo(point:VectorConstant)= (point-pos)*dirUnit
	
	def orthProjection(point:VectorConstant)=	(point-pos)-(dirUnit*getDistanceTo(point))
	
	def orthogonalThrough(point:VectorConstant)= dirUnit*getDistanceTo(point)
	
	def angleBetween(otherDir:VectorConstant)= {
		val ret=90d-dir.angleBetween(otherDir)
		if(ret<0d) -ret else ret
	}
	
	def angleBetween(other:Plane3D)= dir.angleBetween(other.dir)
	
	
	
	def intersectionWith(line:Line3D):VectorConstant = {
		if(isLinearyDependentFrom(line)) 
			throw new IllegalArgumentException("Find intersection, plane "+this+" is parallel with "+line )
    val r=	-1d* (dir*(line.pos-pos))/(dir*line.dir)
    line.pos + line.dir*r
	}
	
	
	def intersectionWith(other:Plane3D):Line3D= {
		if(dir.isLinearyDependentFrom(other.dir))
			throw new IllegalArgumentException("Find intersection, plane "+this+" is parallel with "+other )
		val otherDirProjected=other.dir-dirUnit*(other.dir*dirUnit)
		val startPoint=other.intersectionWith(new Line3D(pos,otherDirProjected))
		new Line3D(startPoint,dir cross other.dir)
	}
	
	def createClone(newPos:VectorConstant,newDir:VectorConstant) = new Plane3D(newPos,newDir)
	
	//def cop(t:VectorConstant,o:Double)= createOffsetPlane(t,o)
	
	
	
	def canEqual(other: Any): Boolean = other.isInstanceOf[Plane3D]
	
	override def equals(other: Any): Boolean =
		other match {
				case that: Plane3D =>
				(that canEqual this) && pos== that.pos && dir==that.dir								
				case _ => false
		}
	
	override def hashCode= pos.hashCode+dir.hashCode*3
	
	/** the x axis coordinate base for a coordinate system on the plane
	 *  it is assumed that this vector is always horizontal
	 */
	lazy val areaAxisX= if(dir.x==0&&dir.y==0) new VectorConstant(1,0,0)
	else VectorConstant.upVector.cross(dir).unit
	
	lazy val areaAxisY= if(dir.x==0&&dir.y==0) new VectorConstant(0,1,0)
	else (VectorConstant.upVector - dirUnit*(VectorConstant.upVector * dirUnit)).unit
  
	/** gets the coordinates of a vector according to the area base
	 *  the point must be on the plane !
	 */
	def getAreaCoords(point:VectorConstant):VectorConstant = {
	  val dist=point-pos
	  if(dist.isNull) NULLVECTOR else
	  if(areaAxisX.z==0&&areaAxisY.z==0){
	    val x=(dist.y*areaAxisY.x-dist.x*areaAxisY.y)/(areaAxisX.y*areaAxisY.x-areaAxisX.x*areaAxisY.y)
	    val y=(dist.y*areaAxisX.x-dist.x*areaAxisX.y)/(areaAxisY.y*areaAxisX.x-areaAxisY.x*areaAxisX.y)
	    new VectorConstant(x,y,0d)
	  } else if(areaAxisX.y==0&&areaAxisY.y==0){
	    val x=(dist.z*areaAxisY.x-dist.x*areaAxisY.z)/(areaAxisX.z*areaAxisY.x-areaAxisX.x*areaAxisY.z)
	    val y=(dist.z*areaAxisX.x-dist.x*areaAxisX.z)/(areaAxisY.z*areaAxisX.x-areaAxisY.x*areaAxisX.z)
	    new VectorConstant(x,y,0d)
	  } else {
	    val x=(dist.z*areaAxisY.y-dist.y*areaAxisY.z)/(areaAxisX.z*areaAxisY.y-areaAxisX.y*areaAxisY.z)
	    val y=(dist.z*areaAxisX.y-dist.y*areaAxisX.z)/(areaAxisY.z*areaAxisX.y-areaAxisY.y*areaAxisX.z)
	    new VectorConstant(x,y,0d)
	  }	   
	}
	
	def toWorldVector(coordsVector:VectorConstant)= pos+areaAxisX*coordsVector.x+areaAxisY*coordsVector.y
}

/*object Plane3D {
	  implicit def builder[A<:Plane3D] : PlaneBuilder[A]=myBuilder
		 val myBuilder=new PlaneBuilder[Plane3D]{ 
			def createClone(oldv:Plane3D,newPos:VectorConstant,newDir:VectorConstant)= new Plane3D(newPos,newDir)
		}
	}*/