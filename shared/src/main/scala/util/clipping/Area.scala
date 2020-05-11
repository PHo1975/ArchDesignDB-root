package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//





import java.awt.geom.Rectangle2D

import scala.collection.mutable.ArrayBuffer

trait PathContainer{
  def getPathIterator():PathIterator
}

object Area {
  private val EmptyCurves = new ArrayBuffer[Curve]()

  private def pathToCurves(pi: PathIterator) = {
    val curves = new ArrayBuffer[Curve]()
    val windingRule = pi.getWindingRule
    val coords: Array[Double] = new Array[Double](23)
    var movx = 0.0D
    var movy = 0.0D
    var curx = 0.0D
    var cury = .0
    cury = 0.0D
    while ( !pi.isDone  ) {
      var newx = .0
      var newy = .0
      pi.currentSegment(coords) match {
        case 0 =>
          Curve.insertLine(curves, curx, cury, movx, movy)
          movx = coords(0)
          curx =  coords(0)
          cury = coords(1)
          movy = coords(1)
          Curve.insertMove(curves, movx, movy)

        case 1 =>
          newx = coords(0)
          newy = coords(1)
          Curve.insertLine(curves, curx, cury, newx, newy)
          curx = newx
          cury = newy

        case 2 =>
          newx = coords(2)
          newy = coords(3)
          Curve.insertQuad(curves, curx, cury, coords)
          curx = newx
          cury = newy

        case 3 =>
          newx = coords(4)
          newy = coords(5)
          Curve.insertCubic(curves, curx, cury, coords)
          curx = newx
          cury = newy

        case 4 =>
          Curve.insertLine(curves, curx, cury, movx, movy)
          curx = movx
          cury = movy
      }

      pi.next()
    }
    Curve.insertLine(curves, curx, cury, movx, movy)
    var operator:AreaOp = null
    if (windingRule == 0) operator = new AreaOp.EOWindOp
    else operator = new AreaOp.NZWindOp
    operator.asInstanceOf[AreaOp].calculate(curves, EmptyCurves)
  }
}

class Area(s:PathContainer) extends Cloneable with PathContainer {
  private var curves:ArrayBuffer[Curve] = Area.pathToCurves(s.getPathIterator())
  //private var cachedBounds:Rectangle2D.Double = null


  def add(rhs: Area): Unit = {
    this.curves = (new AreaOp.AddOp).calculate(this.curves, rhs.curves)
    this.invalidateBounds()
  }

  def subtract(rhs: Area): Unit = {
    this.curves = (new AreaOp.SubOp).calculate(this.curves, rhs.curves)
    this.invalidateBounds()
  }

  def intersect(rhs: Area): Unit = {
    this.curves = (new AreaOp.IntOp).calculate(this.curves, rhs.curves)
    this.invalidateBounds()
  }

  def exclusiveOr(rhs: Area): Unit = {
    this.curves = (new AreaOp.XorOp).calculate(this.curves, rhs.curves)
    this.invalidateBounds()
  }

  def reset(): Unit = {
    this.curves = new ArrayBuffer[Curve]()
    this.invalidateBounds()
  }

  def isEmpty: Boolean = this.curves.size == 0

  def isPolygonal: Boolean = {
    val enum_ = this.curves.iterator
    do if (!enum_.hasNext) return true while ( {
      enum_.next().asInstanceOf[Curve].getOrder <= 1
    })
    false
  }

  def isRectangular: Boolean = {
    val size = this.curves.size
    if (size == 0) true
    else if (size > 3) false
    else {
      val c1 = this.curves(1).asInstanceOf[Curve]
      val c2 = this.curves(2).asInstanceOf[Curve]
      if (c1.getOrder == 1 && c2.getOrder == 1) if (c1.getXTop == c1.getXBot && c2.getXTop == c2.getXBot) c1.getYTop == c2.getYTop && c1.getYBot == c2.getYBot
      else false
      else false
    }
  }

  def isSingular: Boolean = if (this.curves.size < 3) true
  else {
    val enum_ = this.curves.iterator
    enum_.next
    do if (!enum_.hasNext) return true while ( {
      enum_.next.asInstanceOf[Curve].getOrder != 0
    })
    false
  }

  private def invalidateBounds(): Unit = {
    //this.cachedBounds = null
  }

  /*private def getCachedBounds = if (this.cachedBounds != null) this.cachedBounds
  else {
    val r = new Rectangle2D.Double
    if (this.curves.size > 0) {
      val c = this.curves(0).asInstanceOf[Curve]
      r.setRect(c.getX0, c.getY0, 0.0D, 0.0D)
      for (i <- 1 until this.curves.size) {
        this.curves(i).asInstanceOf[Curve].enlarge(r)
      }
    }
    this.cachedBounds = r
    r
  }*/

  /*override def getBounds2D: Rectangle2D = this.getCachedBounds.getBounds2D

  override def getBounds: Rectangle = this.getCachedBounds.getBounds
*/
  override def clone = new Area(this)

  def equals(other: Area): Boolean = if (other eq this) true
  else if (other == null) false
  else {
    val c = (new AreaOp.XorOp).calculate(this.curves, other.curves)
    c.isEmpty
  }

  /*def transform(t: AffineTransform): Unit = {
    if (t == null) throw new NullPointerException("transform must not be null")
    else {
      this.curves = Area.pathToCurves(this.getPathIterator(t))
      this.invalidateBounds()
    }
  }

  def createTransformedArea(t: AffineTransform): Area = {
    val a = new Area(this)
    a.transform(t)
    a
  }*/

  def contains(x: Double, y: Double): Boolean = // if (!this.getCachedBounds.contains(x, y)) false
   {
    val enum_ = this.curves.iterator
    var crossings = 0
    var c:Curve = null
    while (  enum_.hasNext ) {
      c = enum_.next.asInstanceOf[Curve]
      crossings += c.crossingsFor(x, y)
    }
    (crossings & 1) == 1
  }
/*
  override def contains(p: Point2D): Boolean = this.contains(p.getX, p.getY)

  def contains(x: Double, y: Double, w: Double, h: Double): Boolean = if (w >= 0.0D && h >= 0.0D) //if (!this.getCachedBounds.contains(x, y, w, h)) false
  {
    val c = Crossings.findCrossings(this.curves, x, y, x + w, y + h)
    c != null && c.covers(y, y + h)
  }
  else false

  override def contains(r: Rectangle2D): Boolean = this.contains(r.getX, r.getY, r.getWidth, r.getHeight)*/

  def intersects(x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean =
    if (w >= 0.0D && h >= 0.0D)
   {
    val c = Crossings.findCrossings(this.curves, x, y, x + w, y + h)
    c == null || !c.isEmpty
  }
  else false

  def intersects(r: Rectangle2D): Boolean = this.intersects(r.getX, r.getY, r.getWidth, r.getHeight)

  def getPathIterator() =new AreaIterator(this.curves)

  //override def getPathIterator(at: AffineTransform, flatness: Double) = null
}

