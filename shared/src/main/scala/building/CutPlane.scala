package building

import definition.data.{EMPTY_REFERENCE, Referencable, Reference}
import definition.expression._
import util.clipping.Area

class CutPlane(val ref:Reference,val name:String,val pos:VectorConstant,val dir:VectorConstant,val depth:Double,val scale:Int) extends Referencable {
  lazy val plane=new Plane3D(pos,dir)

  def this(nref:Reference,ndata:Seq[Constant])=
    this(nref,ndata(0).toString,ndata(1).toVector,ndata(2).toVector,ndata(3).toDouble,ndata(5).toInt)

  protected val cutShapes=collection.mutable.HashMap[Plane3D,Area]()

  def createCutShape(defPlane:Plane3D): util.clipping.Area ={
    val cutWidth=100d
    if(dir .isLinearyDependentFrom(defPlane.dir)) Polygon.EmptyArea
    else {
      val cutLine=plane.intersectionWith(defPlane)
      val depthLine=new Plane3D(pos+dir.unit*depth,dir).intersectionWith(defPlane)
       new Area(Polygon.toPath2d(Seq(defPlane.getAreaCoords(cutLine.pos+cutLine.dir.unit*cutWidth),
                                     defPlane.getAreaCoords(cutLine.pos-cutLine.dir.unit*cutWidth),
                                     defPlane.getAreaCoords(depthLine.pos-depthLine.dir.unit*cutWidth),
                                     defPlane.getAreaCoords(depthLine.pos+depthLine.dir.unit*cutWidth))))
    }
  }

  def isPointInDepth(point:VectorConstant): Boolean ={
    val dist=plane.getDistanceTo(point)
    dist>=0 && dist<depth
  }

  def getCutShape(defPlane:Plane3D): Area =cutShapes.getOrElseUpdate(defPlane,createCutShape(defPlane))

  override def toString: String =name
}

object NoCutPlane extends CutPlane(EMPTY_REFERENCE,"Alles",NULLVECTOR,NULLVECTOR,0d,0)
