package building

import definition.data.{Referencable, Reference}
import definition.expression.{Constant, IntList}

class Cell(model:AbstractBuildingModel,val ref:Reference,val topPlaneID:Int,val bottomPlaneID:Int,val wallPlaneIDs:Array[Int],val room:Option[Room]) extends Referencable {
  def this(nmodel: AbstractBuildingModel, nref:Reference, ndata: Seq[Constant]) =
    this(nmodel,nref, ndata(0).toInt, ndata(1).toInt,
    ndata(2) match {
      case il: IntList => il.list
      case o => throw new IllegalArgumentException("no IntList in Cell " + o)
    }, nmodel.getRoom(ndata(3).toInt))

  def topPlane=model.getPlane(topPlaneID)
  def bottomPlane=model.getPlane(bottomPlaneID)
  def wallPlane(ix:Int)=model.getPlane(wallPlaneIDs(ix))

  def isLevelPlane(otherPlaneID:Int): Boolean =otherPlaneID==topPlaneID || otherPlaneID== bottomPlaneID


  def iterateWallNeighboars(otherPlaneID:Int): Iterator[Plane] ={
    val wallIx=wallPlaneIDs.indexOf(otherPlaneID)
    if(wallIx == -1) throw new IllegalArgumentException("Plane "+otherPlaneID+" is not part of this cell "+this)
    new Iterator[Plane] {
      var state= 0
      override def hasNext: Boolean = state<4

      override def next(): Plane = {
        state += 1
        model.getPlane(
          state match {
            case 1 => topPlaneID
            case 2 =>if(wallIx==0) wallPlaneIDs.last else wallPlaneIDs(wallIx -1)
            case 3 => bottomPlaneID
            case 4 => if(wallIx==wallPlaneIDs.length-1) wallPlaneIDs.head else wallPlaneIDs(wallIx+1)
          }
        )
      }
    }
  }
}