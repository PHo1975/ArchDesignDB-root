package util.clipping

import scala.collection.mutable.ArrayBuffer

class AreaIterator(var curves: ArrayBuffer[Curve]) extends PathIterator {
  private var thiscurve:Curve = null
  if (curves.size >= 1) this.thiscurve = curves(0).asInstanceOf[Curve]
  private var index = 0
  private var prevcurve:Curve = null


  override def getWindingRule = 1

  override def isDone: Boolean = this.prevcurve == null && this.thiscurve == null

  override def next(): Unit = {
    if (this.prevcurve != null) this.prevcurve = null
    else {
      this.prevcurve = this.thiscurve
      this.index += 1
      if (this.index < this.curves.size) {
        this.thiscurve = this.curves(this.index).asInstanceOf[Curve]
        if (this.thiscurve.getOrder != 0 && this.prevcurve.getX1 == this.thiscurve.getX0 && this.prevcurve.getY1 == this.thiscurve.getY0) this.prevcurve = null
      }
      else this.thiscurve = null
    }
  }

  override def currentSegment(coords: Array[Float]): Int = {
    val dcoords = new Array[Double](6)
    val segtype = this.currentSegment(dcoords)
    val numpoints = if (segtype == 4) 0
    else if (segtype == 2) 2
    else (if (segtype == 3) 3
    else 1)
    for (i <- 0 until numpoints * 2) {
      coords(i) = dcoords(i).toFloat
    }
    segtype
  }

  override def currentSegment(coords: Array[Double]): Int = {
    var segtype = 0
    var numpoints = 0
    if (this.prevcurve != null) {
      if (this.thiscurve == null || this.thiscurve.getOrder == 0) return 4
      coords(0) = this.thiscurve.getX0
      coords(1) = this.thiscurve.getY0
      segtype = 1
      numpoints = 1
    }
    else {
      if (this.thiscurve == null) throw new NoSuchElementException("area iterator out of bounds")
      segtype = this.thiscurve.getSegment(coords)
      numpoints = this.thiscurve.getOrder
      if (numpoints == 0) numpoints = 1
    }
    segtype
  }
}

