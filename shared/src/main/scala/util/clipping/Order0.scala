package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


final class Order0(var x: Double, var y: Double) extends Curve(1) {
  override def getOrder = 0

  override def getXTop: Double = this.x

  override def getYTop: Double = this.y

  override def getXBot: Double = this.x

  override def getYBot: Double = this.y

  override def getXMin: Double = this.x

  override def getXMax: Double = this.x

  override def getX0: Double = this.x

  override def getY0: Double = this.y

  override def getX1: Double = this.x

  override def getY1: Double = this.y

  override def XforY(y: Double): Double = y

  override def TforY(y: Double) = 0.0D

  override def XforT(t: Double): Double = this.x

  override def YforT(t: Double): Double = this.y

  override def dXforT(t: Double, deriv: Int) = 0.0D

  override def dYforT(t: Double, deriv: Int) = 0.0D

  override def nextVertical(t0: Double, t1: Double): Double = t1

  override def crossingsFor(x: Double, y: Double) = 0

  override def accumulateCrossings(c: Crossings): Boolean = this.x > c.getXLo && this.x < c.getXHi && this.y > c.getYLo && this.y < c.getYHi

 /* override def enlarge(r: Rectangle2D): Unit = {
    r.add(this.x, this.y)
  }*/

  override def getSubCurve(ystart: Double, yend: Double, dir: Int): Curve = this

  override def getReversedCurve: Curve = this

  override def getSegment(coords: Array[Double]): Int = {
    coords(0) = this.x
    coords(1) = this.y
    0
  }
}
