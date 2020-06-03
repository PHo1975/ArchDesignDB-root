package building

import definition.expression.{Line3D, VectorConstant}
import util.Log

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

trait AbstractBuildingModel {

  def getPlane(id:Int):Plane

  def getRoom(id:Int):Option[Room]

  def getCell(id:Int): Cell

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
}
