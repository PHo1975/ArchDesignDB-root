package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//

import java.util.Comparator

import scala.collection.mutable.ArrayBuffer


object AreaOp {
  val CTAG_LEFT  = 0
  val CTAG_RIGHT  = 1
  val ETAG_IGNORE  = 0
  val ETAG_ENTER  = 1
  val ETAG_EXIT: Int = - 1
  val RSTAG_INSIDE  = 1
  val RSTAG_OUTSIDE: Int = - 1
  private val YXTopComparator  = new Comparator[Edge]() {
    override def compare(o1: Edge, o2: Edge): Int = {
      val c1  = o1.getCurve
      val c2  = o2.getCurve
      var v1  = .0
      var v2  = .0
      if (({v1 = c1.getYTop;v1}) == ({v2 = c2.getYTop;v2}) && ({v1 = c1.getXTop;v1}) == ({v2 = c2.getXTop;v2})) 0
      else if (v1 < v2) - 1
        else 1
    }
  }
  private val EmptyLinkList  = new Array[CurveLink](2)
  private val EmptyChainList  = new Array[ChainEnd](2)

  private def addEdges(edges: ArrayBuffer[Edge], curves: ArrayBuffer[Curve], curvetag: Int): Unit = {
    val enum_ = curves.iterator
    while (  enum_.hasNext ) {
      val c  = enum_.next.asInstanceOf[Curve]
      if (c.getOrder > 0) edges.addOne(new Edge(c, curvetag))
    }
  }

  def finalizeSubCurves(subcurves: ArrayBuffer[CurveLink], chains: ArrayBuffer[ChainEnd]): Unit = {
    val numchains  = chains.size
    if (numchains != 0) if ((numchains & 1) != 0) throw new InternalError("Odd number of chains!")
      else {
        val endlist  = chains.toArray
        var i  = 1
        while ( i < numchains ) {
          val open  = endlist(i - 1)
          val close  = endlist(i)
          val subcurve  = open.linkTo(close)
          if (subcurve != null) subcurves.addOne(subcurve)

          i += 2
        }
        chains.clear()
      }
  }

  def resolveLinks(subcurves: ArrayBuffer[CurveLink], chains: ArrayBuffer[ChainEnd], links: ArrayBuffer[CurveLink]): Unit = {
    val numlinks  = links.size
    var linklist :Array[CurveLink] = null
    if (numlinks == 0) linklist = EmptyLinkList
    else {
      if ((numlinks & 1) != 0) throw new InternalError("Odd number of new curves!")
      linklist = new Array[CurveLink](numlinks + 2)
      for(i<-links.indices) linklist(i)=links(i)
    }
    val numchains  = chains.size
    var endlist :Array[ChainEnd] = null
    if (numchains == 0) endlist = EmptyChainList
    else {
      if ((numchains & 1) != 0) throw new InternalError("Odd number of chains!")
      endlist = new Array[ChainEnd](numchains + 2)
      for(i<-chains.indices) endlist(i)=chains(i)
    }
    var curchain  = 0
    var curlink  = 0
    chains.clear()
    var chain  = endlist(0)
    var nextchain  = endlist(1)
    var link  = linklist(0)
    var nextlink  = linklist(1)
    while ( {
      chain != null || link != null
    }) {
      var connectchains  = link == null
      var connectlinks  = chain == null
      if (! connectchains && ! connectlinks ) {
        connectchains = (curchain & 1) == 0 && chain.getX == nextchain.getX
        connectlinks = (curlink & 1) == 0 && link.getX == nextlink.getX
        if (! connectchains && ! connectlinks ) {
          val cx  = chain.getX
          val lx  = link.getX
          connectchains = nextchain != null && cx < lx && obstructs(nextchain.getX, lx, curchain)
          connectlinks = nextlink != null && lx < cx && obstructs(nextlink.getX, cx, curlink)
        }
      }
      if (connectchains) {
        val subcurve  = chain.linkTo(nextchain)
        if (subcurve != null) subcurves.addOne(subcurve)
        curchain += 2
        chain = endlist(curchain)
        nextchain = endlist(curchain + 1)
      }
      if (connectlinks) {
        val openend  = new ChainEnd(link, null.asInstanceOf[ChainEnd])
        val closeend  = new ChainEnd(nextlink, openend)
        openend.setOtherEnd(closeend)
        chains.addOne(openend)
        chains.addOne(closeend)
        curlink += 2
        link = linklist(curlink)
        nextlink = linklist(curlink + 1)
      }
      if (! connectchains && ! connectlinks ) {
        chain.addLink(link)
        chains.addOne(chain)
        curchain += 1
        chain = nextchain
        nextchain = endlist(curchain + 1)
        curlink += 1
        link = nextlink
        nextlink = linklist(curlink + 1)
      }
    }
    if ((chains.size & 1) != 0) System.out.println("Odd number of chains!")
  }

  def obstructs(v1: Double, v2: Double, phase: Int): Boolean = if ((phase & 1) == 0) v1 <= v2
    else v1 < v2

  class EOWindOp() extends AreaOp {
    private var inside  = false

    override def newRow(): Unit = {
      this.inside = false
    }

    override def classify(e: Edge): Int = {
      val newInside  = ! this.inside
      this.inside = newInside
      if (newInside) 1
      else - 1
    }

    override def getState: Int = if (this.inside) 1
      else - 1
  }

  class NZWindOp() extends AreaOp {
    private var count  = 0

    override def newRow(): Unit = {
      this.count = 0
    }

    override def classify(e: Edge): Int = {
      var newCount  = this.count
      val `type`  = if (newCount == 0) 1
      else 0
      newCount += e.getCurve.getDirection
      this.count = newCount
      if (newCount == 0) - 1
      else `type`
    }

    override def getState: Int = if (this.count == 0) - 1
      else 1
  }

  class XorOp() extends AreaOp.CAGOp {
    override def newClassification(inLeft: Boolean, inRight: Boolean): Boolean = inLeft != inRight
  }

  class IntOp() extends AreaOp.CAGOp {
    override def newClassification(inLeft: Boolean, inRight: Boolean): Boolean = inLeft && inRight
  }

  class SubOp() extends AreaOp.CAGOp {
    override def newClassification(inLeft: Boolean, inRight: Boolean): Boolean = inLeft && ! inRight
  }

  class AddOp() extends AreaOp.CAGOp {
    override def newClassification(inLeft: Boolean, inRight: Boolean): Boolean = inLeft || inRight
  }

  abstract class CAGOp() extends AreaOp {
     var inLeft  = false
     var inRight  = false
     var inResult  = false

    override def newRow(): Unit = {
      this.inLeft = false
      this.inRight = false
      this.inResult = false
    }

    override def classify(e: Edge): Int = {
      if (e.getCurveTag == 0) this.inLeft = ! this.inLeft
      else this.inRight = ! this.inRight
      val newClass  = this.newClassification(this.inLeft, this.inRight)
      if (this.inResult == newClass) 0
      else {
        this.inResult = newClass
        if (newClass) 1
        else - 1
      }
    }

    override def getState: Int = if (this.inResult) 1
      else - 1

    def newClassification(var1: Boolean, var2: Boolean): Boolean
  }

}

abstract class AreaOp private() {
  def newRow(): Unit

  def classify(var1: Edge): Int

  def getState: Int

  def calculate(left: ArrayBuffer[Curve], right: ArrayBuffer[Curve]): ArrayBuffer[Curve] = {
    val edges  = new ArrayBuffer[Edge]()
    AreaOp.addEdges(edges, left, 0)
    AreaOp.addEdges(edges, right, 1)
    val curves  = this.pruneEdges(edges)
    curves
  }


  private def pruneEdges(edges: ArrayBuffer[Edge])  = {
    val numedges  = edges.size
    if (numedges < 2) new ArrayBuffer[Curve]
    else {
      val edgelist  = edges.toArray
      java.util.Arrays.sort(edgelist, AreaOp.YXTopComparator)
      var left  = 0
      var right  = 0
      val cur  = false
      val next  = false
      val yrange  = new Array[Double](2)
      val subcurves  = new ArrayBuffer[CurveLink]
      val chains  = new ArrayBuffer[ChainEnd]
      var yend  = .0
      val links  = new ArrayBuffer[CurveLink]
      var dobreak = false
      while ( left < numedges&& (!dobreak)  ) {

        var y = yrange(0)
        var e: Edge = null
        var cur = 0
        var next = 0
        cur = right - 1
        next = right - 1
        while (cur >= left) {
          e = edgelist(cur)
          if (e.getCurve.getYBot > y) {
            if (next > cur) edgelist(next) = e
            next -= 1
          }
          cur -= 1
        }
        left = next + 1
        if (left >= right) {
          if (right >= numedges) dobreak = true //todo: break is not supported
          else {
            y = edgelist(right).getCurve.getYTop
            if (y > yrange(0)) AreaOp.finalizeSubCurves(subcurves, chains)
            yrange(0) = y
          }
        }
        if(!dobreak ){
          var goWhile=true
          while ( right < numedges&& goWhile ) {
            e = edgelist(right)
            if (e.getCurve.getYTop > y) goWhile=false //todo: break is not supported
            else right += 1
          }
          yrange(1) = edgelist(left).getCurve.getYBot
          if (right < numedges) {
            y = edgelist(right).getCurve.getYTop
            if (yrange(1) > y) yrange(1) = y
          }
          var nexteq = 1
          cur = left
          while (cur < right) {
            e = edgelist(cur)
            e.setEquivalence(0)
            next = cur
            var gowhile1=true
            while (next > left&& gowhile1) {
              val prevedge = edgelist(next - 1)
              val ordering = e.compareTo(prevedge, yrange)
              if (yrange(1) <= yrange(0)) throw new InternalError("backstepping to " + yrange(1) + " from " + yrange(0))
              if (ordering >= 0) {
                if (ordering == 0) {
                  var eq = prevedge.getEquivalence
                  if (eq == 0) {
                    eq = {
                      nexteq += 1;
                      nexteq - 1
                    }
                    prevedge.setEquivalence(eq)
                  }
                  e.setEquivalence(eq)
                }
                gowhile1=false

              }
              if(gowhile1){
                edgelist(next) = prevedge
                next -= 1
              }
            }
            edgelist(next) = e

            cur += 1
          }
          this.newRow()
          val ystart = yrange(0)
          yend = yrange(1)
          var eq = 0
          cur = left
          while (cur < right) {
            e = edgelist(cur)
            var eq = e.getEquivalence
            if (eq == 0) eq = this.classify(e)
            else {
              val origstate = this.getState
              eq = if (origstate == 1) -1
              else 1
              var activematch: Edge = null
              var longestmatch = e
              var furthesty = yend
              do {
                this.classify(e)
                if (activematch == null && e.isActiveFor(ystart, eq)) activematch = e
                y = e.getCurve.getYBot
                if (y > furthesty) {
                  longestmatch = e
                  furthesty = y
                }
                cur += 1
              } while (
                cur < right && ({
                  e = edgelist(cur);
                  e
                }).getEquivalence == eq
              )
              cur -= 1
              if (this.getState == origstate) eq = 0
              else e = if (activematch != null) activematch
              else longestmatch
            }
            if (eq != 0) {
              e.record(yend, eq)
              links.addOne(new CurveLink(e.getCurve, ystart, yend, eq))
            }

            cur += 1
          }
          if (this.getState != -1) {
            System.out.println("Still inside at end of active edge list!")
            System.out.println("num curves = " + (right - left))
            System.out.println("num links = " + links.size)
            System.out.println("y top = " + yrange(0))
            if (right < numedges) System.out.println("y top of next curve = " + edgelist(right).getCurve.getYTop)
            else System.out.println("no more curves")
            cur = left
            while ( {
              cur < right
            }) {
              e = edgelist(cur)
              System.out.println(e)
              eq = e.getEquivalence
              if (eq != 0) System.out.println("  was equal to " + eq + "...")

              cur += 1
            }
          }
          AreaOp.resolveLinks(subcurves, chains, links)
          links.clear()

          yrange(0) = yend
      }
      }
      AreaOp.finalizeSubCurves(subcurves, chains)
      val ret  = new ArrayBuffer[Curve]
      val enum_ = subcurves.iterator
      while (  enum_.hasNext  ) {
        var link: CurveLink = enum_.next.asInstanceOf[CurveLink]
        ret.addOne(link.getMoveto)
        var nextlink: CurveLink = link
        while (
          {nextlink = nextlink.getNext;nextlink} != null
        ) if (! link.absorb(nextlink) ) {
            ret.addOne(link.getSubCurve)
            link = nextlink
          }
        ret.addOne(link.getSubCurve)
      }
      ret
    }
  }
}
