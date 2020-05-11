package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


object Edge {
  val INIT_PARTS = 4
  val GROW_PARTS = 10
}

final class Edge(var curve: Curve, var ctag: Int, var etag: Int) {
   var activey = .0
  var equivalence = 0
  private var lastEdge:Edge = null
  private var lastResult = 0
  private var lastLimit = .0

  def this(c: Curve, ctag: Int) {
    this(c, ctag, 0)
  }

  def getCurve: Curve = this.curve

  def getCurveTag: Int = this.ctag

  def getEdgeTag: Int = this.etag

  def setEdgeTag(etag: Int): Unit = {
    this.etag = etag
  }

  def getEquivalence: Int = this.equivalence

  def setEquivalence(eq: Int): Unit = {
    this.equivalence = eq
  }

  def compareTo(other: Edge, yrange: Array[Double]): Int = if ((other eq this.lastEdge) && yrange(0) < this.lastLimit) {
    if (yrange(1) > this.lastLimit) yrange(1) = this.lastLimit
    this.lastResult
  }
  else if ((this eq other.lastEdge) && yrange(0) < other.lastLimit) {
    if (yrange(1) > other.lastLimit) yrange(1) = other.lastLimit
    0 - other.lastResult
  }
  else {
    val ret = this.curve.compareTo(other.curve, yrange)
    this.lastEdge = other
    this.lastLimit = yrange(1)
    this.lastResult = ret
    ret
  }

  def record(yend: Double, etag: Int): Unit = {
    this.activey = yend
    this.etag = etag
  }

  def isActiveFor(y: Double, etag: Int): Boolean = this.etag == etag && this.activey >= y

  override def toString: String = {
    val var10000 = this.curve
    "Edge[" + var10000 + ", " + (if (this.ctag == 0) "L"
    else "R") + ", " + (if (this.etag == 1) "I"
    else if (this.etag == -(1)) "O"
    else "N") + "]"
  }
}
