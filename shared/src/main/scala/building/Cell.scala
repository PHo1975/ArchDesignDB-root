package building

import definition.data.{Referencable, Reference}
import definition.expression.{Constant, IntList}

class Cell(model:AbstractBuildingModel,val ref:Reference,val topPlaneID:Int,val bottomPlaneID:Int,val wallPlaneIDs:Array[Int],
           val roomID:Int) extends Referencable {
  def this(nmodel: AbstractBuildingModel, nref:Reference, ndata: Seq[Constant]) =
    this(nmodel,nref, ndata(0).toInt, ndata(1).toInt,
    ndata(2) match {
      case il: IntList => il.list
      case o => throw new IllegalArgumentException("no IntList in Cell " + o)
    }, ndata(3).toInt)

  def topPlane: Plane =model.getPlane(topPlaneID)
  def bottomPlane: Plane =model.getPlane(bottomPlaneID)
  def wallPlane(ix:Int): Plane =model.getPlane(wallPlaneIDs(ix))

  def isLevelPlane(otherPlaneID:Int): Boolean =otherPlaneID==topPlaneID || otherPlaneID== bottomPlaneID

  def iterateWallNeighboars(otherPlaneID:Int): Iterator[Plane] ={
    val wallIx=wallPlaneIDs.indexOf(otherPlaneID)
    if(wallIx == -1) throw new IllegalArgumentException("iterate walls Plane "+otherPlaneID+" is not part of this cell "+
      this.toString)
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

  override def toString:String=this.getClass.getSimpleName+"("+ref+" top:"+topPlaneID+" bottom:"+bottomPlaneID+" walls:"+wallPlaneIDs.mkString(",")+")"

  def loopPlanes: Iterator[Int] = new Iterator[Int] {
    var state: Int = -1
    val max: Int =wallPlaneIDs.length+2
    override def hasNext: Boolean = state<max

    override def next(): Int = {
      state+=1
      if(state==0) topPlaneID
      else if(state==1) bottomPlaneID
      else if(state<max) wallPlaneIDs(state-2) else -1
    }
  }

  def isConnectedTo(otherCell:Cell,allPartAreas:Iterable[PartArea]):Boolean=
    allPartAreas.exists(p=>(p.firstCellID==ref.instance&&p.secondCellID==otherCell.ref.instance) ||
      (p.firstCellID==otherCell.ref.instance&&p.secondCellID==ref.instance))

  def findPartAreaTo(otherCell:Cell,allPartAreas:Iterable[PartArea]):Option[PartArea]= {
    allPartAreas.find(p=>(p.firstCellID==ref.instance&&p.secondCellID==otherCell.ref.instance) ||
      (p.firstCellID==otherCell.ref.instance&&p.secondCellID==ref.instance))
  }

}