package building

import definition.data.{Referencable, Reference}
import definition.expression._
import util.Log
import util.clipping.Area

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

class PartArea(model: AbstractBuildingModel,val ref:Reference,val defPlaneID:Int,val firstCellID:Int,val secondCellID:Int,
               val aufbau:Int,val align:Double) extends Referencable {
  def this(nref:Reference,ndata:Seq[Constant],nmodel: AbstractBuildingModel)={
    this(nmodel,nref,ndata(0).toInt,ndata(1).toInt,
      ndata(2).toInt,ndata(3).toInt,ndata(4).toDouble)
  }

  def defPlane: Plane =model.getPlane(defPlaneID)

  override def toString: String ="PartArea "+ref+" defplane:"+defPlaneID+" firstCell:"+firstCellID+" secondCell:"+secondCellID


  def pointsFromEdges(edges:Iterator[Line3D]): Iterator[VectorConstant] = try{
    val points=ArrayBuffer[VectorConstant]()
    val firstLine=edges.next()
    var lastLine:Line3D=firstLine
    for(e<-edges) {
      points.append(e.intersectionWith(lastLine))
      lastLine=e
    }
    points.append(firstLine.intersectionWith(lastLine))
    points.iterator
  } catch {
    case NonFatal(e)=> Log.e("PartArea "+this.toString,e) ;Iterator.empty
  }

  def createCornerPoints(cutPlane:CutPlane): Iterator[VectorConstant] ={
    val defPlane3D=defPlane.plane
    var cutShape:Area= null

    def cutPoints(points:Iterator[VectorConstant]): Iterator[VectorConstant] ={
      if(cutShape!=null&& cutShape!= Polygon.EmptyArea) {
        val narea=new Area(Polygon.toPath2d(points))
        narea.intersect(cutShape)
        val pointLists=Polygon.newAreaToPoints(narea)
        if(pointLists.isEmpty) Iterator.empty else {
          val pl=pointLists.head.points
          if(pl.head==pl.last)pl.drop(1).iterator else pl.iterator
        }
      } else points
    }

    if(cutPlane!=NoCutPlane){
      cutShape=cutPlane.getCutShape(defPlane3D)
      if(cutShape==Polygon.EmptyArea&& // defPlane is parallel to cutPlane
        !cutPlane.isPointInDepth(defPlane3D.pos)) // and is not in range
        return Iterator.empty
    }
    val firstCell=model.getCell(firstCellID)
    val secondCell=if(secondCellID==0)None else Option(model.getCell(secondCellID))
    if(firstCell.isLevelPlane(defPlaneID)) {
      secondCell match {
        case Some(secCell)=>
          val firstCellPoints  = PointList(pointsFromEdges(firstCell.wallPlaneIDs.iterator.map(
            p=>defPlane3D.intersectionWith(model.getPlane(p).plane))).map(defPlane3D.getAreaCoords).toSeq).conterClockWise
          val secondCellPoints=PointList(pointsFromEdges(secCell.wallPlaneIDs.iterator.map(
            p=>defPlane3D.intersectionWith(model.getPlane(p).plane))).map(defPlane3D.getAreaCoords).toSeq).conterClockWise
          val intersection =VectorConstant.intersectShapes2d(firstCellPoints.points:+firstCellPoints.points.head,secondCellPoints.points:+secondCellPoints.points.head)
          val result: Seq[VectorConstant] =if(intersection.nonEmpty&&intersection.last==intersection.head) intersection.dropRight(1) else intersection
          cutPoints(result.iterator)
        case None =>
          val points=pointsFromEdges(firstCell.wallPlaneIDs.iterator.map(p=>defPlane3D.intersectionWith(model.getPlane(p).plane)))
          val mapped: Iterator[VectorConstant] =points.map(defPlane3D.getAreaCoords)
          if(cutShape!=null&&cutShape!=Polygon.EmptyArea)
            cutPoints(mapped) else mapped
      }
    } else { //wallplane
      secondCell match {
        case Some(secCell) =>
          val firstCellPoints = PointList(pointsFromEdges(firstCell.iterateWallNeighboars(defPlaneID).map(
            p => defPlane3D.intersectionWith(p.plane))).map(defPlane3D.getAreaCoords).toSeq).conterClockWise
          val secondCellPoints = PointList(pointsFromEdges(secCell.iterateWallNeighboars(defPlaneID).map(
            p => defPlane3D.intersectionWith(p.plane))).map(defPlane3D.getAreaCoords).toSeq).conterClockWise
          val intersection = VectorConstant.intersectShapes2d(firstCellPoints.points :+ firstCellPoints.points.head, secondCellPoints.points :+ secondCellPoints.points.head)
          val result = if (intersection.nonEmpty&&intersection.last == intersection.head) intersection.dropRight(1) else intersection
          cutPoints(result.iterator)
        case None =>
          val points=pointsFromEdges(firstCell.iterateWallNeighboars(defPlaneID).map(p => defPlane.plane.intersectionWith(p.plane)))
          val mapped: Iterator[VectorConstant] =points.map(defPlane3D.getAreaCoords)
          if(cutShape!=null&&cutShape!=Polygon.EmptyArea)
            cutPoints(mapped) else mapped
      }
    }
  }
}
