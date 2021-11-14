package definition.expression

import util.Log

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double.TotalOrdering


protected trait Node{
  var next:Node

  @tailrec
  final def nextVertex:Vertex=next match {
    case v:Vertex=>v
    case o=> o.nextVertex
  }

  def join(other:Node): Unit ={
    other.next=next
    next=other
  }

  def iterator:Iterator[Node]= new Iterator[Node]{
    var loopvar:Node=Node.this
    var run=true
    override def hasNext: Boolean = run
    override def next(): Node = {
      val result=loopvar
      loopvar=loopvar.next
      if(loopvar==Node.this)run=false
      result
    }
  }
}

protected trait Vertex extends Node {
  def point:VectorConstant
  override def toString: String =getClass.getSimpleName+" p:"+point

  def vertexIterator:Iterator[Vertex]= new Iterator[Vertex]{
    var loopvar:Node=Vertex.this
    var run=true

    override def hasNext: Boolean =run
    override def next(): Vertex ={
      val result=loopvar match {
        case v:Vertex=> v
        case o=> loopvar=o.nextVertex;o.nextVertex
      }
      loopvar=loopvar.nextVertex
      if(loopvar==Vertex.this) run=false
      result
    }
  }
}


protected class KnifeVertex(val point:VectorConstant,var next:Node,val id:Int) extends Vertex{
  override def toString: String ="KnifeVertex id:"+id+" p:"+point
}
protected class CakeVertexOutside(val point:VectorConstant, var next:Node) extends Vertex
class CakeVertexInside(val point:VectorConstant, var next:Node) extends Vertex

protected class CakeIntersectInbound(val point:VectorConstant, var next:Node, var knife:KnifeInbound) extends Vertex{
  def cutIterator: Iterator[Vertex] =new Iterator[Vertex] {
    var loopVar: Node =knife.next
    var run:Boolean=true
    override def hasNext: Boolean = run
    override def next(): Vertex = {
      val result=loopVar match {
        case k:KnifeVertex=>loopVar=k.next;k
        case o:KnifeOutbound=> loopVar=o.cake.next;o.cake
        case i:KnifeInbound=> throw new IllegalStateException("Wrong Typology, unexpected KnifeInbound "+i+" starting From "+CakeIntersectInbound.this )
        case c:CakeIntersectInbound=> loopVar=c.knife.next;c
        case ci:CakeVertexInside=> println("cut loop illegaly reaches "+ci);loopVar=ci.next;ci
        case co:CakeVertexOutside=> loopVar=co.next;co
      }
      if(result==CakeIntersectInbound.this) run=false
      result
    }
  }
  def intersectIterator: Iterator[Vertex] =new Iterator[Vertex] {
    var loopVar: Node =CakeIntersectInbound.this.next
    var run:Boolean=true
    override def hasNext: Boolean = run
    override def next(): Vertex = {
      val result=loopVar match {
        case k:KnifeVertex=>loopVar=k.next;k
        case o:KnifeOutbound=>throw new IllegalStateException("Wrong Typology, unexpected KnifeInbound "+o+" starting From "+CakeIntersectInbound.this )
        case i:KnifeInbound=> loopVar=i.cake.next;i.cake
        case c:CakeIntersectInbound=> loopVar=c.next;c
        case cio:CakeIntersectOutbound=>loopVar=cio.knife.next;cio
        case ci:CakeVertexInside=> loopVar=ci.next;ci
        case co:CakeVertexOutside=> println("cut loop illegaly reaches "+co);loopVar=co.next;co
      }
      if(loopVar==CakeIntersectInbound.this.next) run=false
      result
    }
  }


}
protected class CakeIntersectOutbound(val point:VectorConstant, var next:Node, var knife:KnifeOutbound) extends Vertex
protected class KnifeInbound(var next:Node,var cake:CakeIntersectInbound) extends Node {
  override def toString: String ="KnifeInbound connect to:"+cake
}
protected class KnifeOutbound(var next:Node,var cake:CakeIntersectOutbound) extends Node {
  override def toString: String ="KnifeOutbound connect to:"+cake
}



object WeilerAthertonClipping {

  sealed trait ClipResult

  case class HaveClip(cakeLoop:Vertex,kniveLoop:Node) extends ClipResult

  object NoIntersection extends ClipResult
  object KnifeIsHole extends ClipResult
  object KnifeOverlaps extends ClipResult


  protected def nodeInside(node: Vertex): Boolean = node match {
    case c: CakeVertexInside => true
    case o: CakeVertexOutside => false
    case n => throw new IllegalArgumentException(" wrong Node Type")
  }

  protected class KnifeRing(knifeShape: PointList) {
    val knifeHead = new KnifeVertex(knifeShape.points.head, null, 0)

    def buildLoop(): Unit ={
      var knifeLoop = knifeHead
      for (i <- 1 until knifeShape.points.size; k = knifeShape.points(i)) {
        val nextNode = new KnifeVertex(k, null, i)
        knifeLoop.next = nextNode
        knifeLoop = nextNode
      }
      knifeLoop.next = knifeHead
    }
    buildLoop()

    protected def addAfterVertex(newNode: Node,position:VectorConstant, vid: Int): Unit = if (vid < knifeShape.points.size) {
      var loopVar: Node = knifeHead
      // find Vertex at Position vid
      while (loopVar match {
        case k: KnifeVertex => k.id != vid
        case _ => true
      }) loopVar = loopVar.next
      val p1=loopVar.asInstanceOf[KnifeVertex].point
      // find next Vertex
      var secondLoopVar:Node=loopVar.next
      while (secondLoopVar match {
        case k: KnifeVertex => false
        case _ => true
      }) secondLoopVar = secondLoopVar.next
      val p2=secondLoopVar.asInstanceOf[KnifeVertex].point
      //println("Calc dist p1:"+p1+" s:"+position+" p2:"+p2)
      val newNodeDistance=position.dividesSegment(p1,p2)
      secondLoopVar=loopVar // first element after vid
      while(secondLoopVar.next match {
        case _:KnifeVertex=>false
        case ki:KnifeInbound=>ki.cake.point.dividesSegment(p1,p2)<newNodeDistance
        case ko:KnifeOutbound=>ko.cake.point.dividesSegment(p1,p2)<newNodeDistance
      }) secondLoopVar=secondLoopVar.next
      newNode.next = secondLoopVar.next
      secondLoopVar.next = newNode
    }

    def findIntersections(a: VectorConstant, b: VectorConstant, aIsInside: Boolean): Seq[Vertex] = {
      //val intersections = ArrayBuffer[(VectorConstant, Int, VectorConstant, Boolean)]()
      val intersections: Seq[(Int, VectorConstant)] =knifeHead.vertexIterator.collect{
        case k:KnifeVertex=>
          VectorConstant.intersectInSegment2DExclusive(a, b, k.point, k.nextVertex.point).map(schnittpunkt=>
             (/*k.point,*/ k.id, // next Knife Vertex ID
              schnittpunkt/*, VectorConstant.insideShape(k.point, cakeShape.points)*/))
          }.flatten.toSeq.sortBy(f => f._2.dividesSegment(a, b))(TotalOrdering)

      println("Intersections: in "+a+" to "+b+":\n "+intersections.mkString("\n "))
      var moveInKnife = !aIsInside
      var cakeConnect: Vertex = null
      for ((pointNr,interPoint) <- intersections) yield {
        val newNode: Vertex = if (moveInKnife) {
          val ki = new KnifeInbound(null, null)
          val ci = new CakeIntersectInbound(interPoint, null, ki)
          ki.cake = ci
          addAfterVertex(ki,interPoint, pointNr)
          ci
        }
        else {
          val ko = new KnifeOutbound(null, null)
          val co = new CakeIntersectOutbound(interPoint, null, ko)
          ko.cake = co
          addAfterVertex(ko,interPoint, pointNr)
          co
        }
        if (cakeConnect != null) cakeConnect.next = newNode
        cakeConnect = newNode
        moveInKnife = !moveInKnife
        newNode
      }
    }
  }

  protected def createPointList(cake:Vertex,getLoopIterator:CakeIntersectInbound=>Iterator[Vertex]): Seq[PointList] ={
    val inboundPoints=new ArrayBuffer[CakeIntersectInbound]()
    inboundPoints++= cake.vertexIterator.filter(_ match { case _: CakeIntersectInbound => true case _ => false }).
      iterator.collect { case e: CakeIntersectInbound => e }
    if (inboundPoints.isEmpty) {Log.e("No Inboundpoints");Seq.empty}
    else {
      println("Inboundpoints:"+inboundPoints.mkString(" | "))
      var partList:List[PointList]=Nil

      while (inboundPoints.nonEmpty){
        val iterator: Iterator[Vertex] =getLoopIterator(inboundPoints.head)
        partList=PointList(iterator.map {
          case e: CakeIntersectInbound =>println("L:"+e)
            if(!inboundPoints.contains(e)){
              Log.e("Cant find Inbound "+e+" in List \n"+inboundPoints.mkString("|"))
              inboundPoints.clear()
            }
          else inboundPoints -= e; e.point
          case o => println("L:"+o); o.point
        }.toSeq) :: partList
      }
      println("Result:\n"+partList.mkString("\n"))
      partList
    }
  }

  def cut(cakeShape: PointList, knifeShape: PointList): Seq[PointList] =
    if (cakeShape.numVecs < 3 || knifeShape.numVecs < 3) Seq(cakeShape) else
      if (cakeShape.isSameAs(knifeShape)) Seq.empty
    else {
     clipShapes(cakeShape, knifeShape.reverse) match {
       case NoIntersection=> Seq(cakeShape)
       case KnifeIsHole=> Seq(cakeShape,knifeShape.reverse)
       case HaveClip(cake,knife)=>
         println("cake list \n"+cake.vertexIterator.mkString("\n"))
         println("\nknife list\n"+knife.iterator.mkString("\n"))
         createPointList(cake,_.cutIterator)
       case KnifeOverlaps=>Seq.empty
     }
    }

  def union(cakeShape: PointList, knifeShape: PointList): Seq[PointList] =
    if (cakeShape.numVecs < 3 || knifeShape.numVecs < 3) Seq(cakeShape) else
      if (cakeShape.isSameAs(knifeShape)) Seq(cakeShape)
      else {
        clipShapes(cakeShape, knifeShape) match {
          case NoIntersection=> Seq(cakeShape,knifeShape)
          case KnifeIsHole=> Seq(cakeShape)
          case HaveClip(cake,knife)=>
            println("cake list \n"+cake.vertexIterator.mkString("\n"))
            println("\nknife list\n"+knife.iterator.mkString("\n"))
            createPointList(cake,_.cutIterator)
          case KnifeOverlaps=>Seq(knifeShape)
        }
      }

  def intersect(cakeShape: PointList, knifeShape: PointList): Seq[PointList] =
    if (cakeShape.numVecs < 3 || knifeShape.numVecs < 3) Seq.empty else
      if (cakeShape.isSameAs(knifeShape)) Seq(cakeShape)
      else {
        clipShapes(cakeShape.reverse, knifeShape.reverse) match {
          case NoIntersection=> Seq.empty
          case KnifeIsHole=> Seq(knifeShape)
          case HaveClip(cake,knife)=>
            println("cake list \n"+cake.vertexIterator.mkString("\n"))
            println("\nknife list\n"+knife.iterator.mkString("\n"))
            createPointList(cake,_.intersectIterator)
          case KnifeOverlaps=>Seq(cakeShape)
        }
      }


  protected def clipShapes(cakeShape:PointList,knifeShape:PointList):ClipResult = {
    //val orderedCakeShape=cakeShape.conterClockWise
    val knifeRing=new KnifeRing(knifeShape)
    //val knifeConterClock=knifeShape.conterClockWise

    var numInsidePoints=0

    def checkInside(p:VectorConstant,kniveShape:PointList): Vertex =
      if (VectorConstant.insideShape(p,knifeShape.points)) {
        numInsidePoints+=1
        new CakeVertexInside(p,null)
      }
      else new CakeVertexOutside(p,null)


    // create CakeLoop, mark inside and outside Vertices
    val cakeHead=checkInside(cakeShape.points.head,cakeShape)
    var loopVar:Vertex=cakeHead
    for (i<-1 until cakeShape.numVecs;c=cakeShape.points(i)){
      val nextNode=checkInside(c,cakeShape)
      loopVar.next=nextNode
      loopVar=nextNode
    }
    loopVar.next=cakeHead

    loopVar = cakeHead
    var hasIntersections:Boolean=false
    do {
      val nextPoint=loopVar.next.asInstanceOf[Vertex]
      val nowInside = nodeInside(loopVar)
      val intersections=knifeRing.findIntersections(loopVar.point, nextPoint.point, nowInside)
      if(intersections.nonEmpty) {
        hasIntersections=true
        intersections.last.next = nextPoint // add intersections in loop
        loopVar.next = intersections.head
      }
      loopVar = nextPoint
    } while (loopVar != cakeHead)
    if(!hasIntersections) {
      if(numInsidePoints==cakeShape.numVecs) KnifeOverlaps
      else if( knifeRing.knifeHead.vertexIterator.count(p=>VectorConstant.insideShape(p.point,cakeShape.points))==knifeShape.numVecs)
        KnifeIsHole
      else NoIntersection
    } else HaveClip(loopVar,knifeRing.knifeHead)
  }
}