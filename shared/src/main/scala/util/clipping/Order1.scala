package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//



final class Order1(var x0: Double, var y0: Double, var x1: Double, var y1: Double, ndirection: Int) extends Curve(ndirection) {
  private var xmin = .0
  private var xmax = .0

  if (x0 < x1) {
    this.xmin = x0
    this.xmax = x1
  }
  else {
    this.xmin = x1
    this.xmax = x0
  }

  override def getOrder = 1

  override def getXTop: Double = this.x0

  override def getYTop: Double = this.y0

  override def getXBot: Double = this.x1

  override def getYBot: Double = this.y1

  override def getXMin: Double = this.xmin

  override def getXMax: Double = this.xmax

  override def getX0: Double = if (this.direction == 1) this.x0
  else this.x1

  override def getY0: Double = if (this.direction == 1) this.y0
  else this.y1

  override def getX1: Double = if (this.direction == -1) this.x0
  else this.x1

  override def getY1: Double = if (this.direction == -1) this.y0
  else this.y1

  override def XforY(y: Double): Double = if (this.x0 != this.x1 && y > this.y0) if (y >= this.y1) this.x1
  else this.x0 + (y - this.y0) * (this.x1 - this.x0) / (this.y1 - this.y0)
  else this.x0

  override def TforY(y: Double): Double = if (y <= this.y0) 0.0D
  else if (y >= this.y1) 1.0D
  else (y - this.y0) / (this.y1 - this.y0)

  override def XforT(t: Double): Double = this.x0 + t * (this.x1 - this.x0)

  override def YforT(t: Double): Double = this.y0 + t * (this.y1 - this.y0)

  override def dXforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      this.x0 + t * (this.x1 - this.x0)
    case 1 =>
      this.x1 - this.x0
    case _ =>
      0.0D
  }

  override def dYforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      this.y0 + t * (this.y1 - this.y0)
    case 1 =>
      this.y1 - this.y0
    case _ =>
      0.0D
  }

  override def nextVertical(t0: Double, t1: Double): Double = t1

  override def accumulateCrossings(c: Crossings): Boolean = {
    val xlo = c.getXLo
    val ylo = c.getYLo
    val xhi = c.getXHi
    val yhi = c.getYHi
    if (this.xmin >= xhi) false
    else {
      var xstart = .0
      var ystart = .0
      if (this.y0 < ylo) {
        if (this.y1 <= ylo) return false
        ystart = ylo
        xstart = this.XforY(ylo)
      }
      else {
        if (this.y0 >= yhi) return false
        ystart = this.y0
        xstart = this.x0
      }
      var yend = .0
      var xend = .0
      if (this.y1 > yhi) {
        yend = yhi
        xend = this.XforY(yhi)
      }
      else {
        yend = this.y1
        xend = this.x1
      }
      if (xstart >= xhi && xend >= xhi) false
      else if (xstart <= xlo && xend <= xlo) {
        c.record(ystart, yend, this.direction)
        false
      }
      else true
    }
  }

  /*override def enlarge(r: Rectangle2D): Unit = {
    r.add(this.x0, this.y0)
    r.add(this.x1, this.y1)
  }*/

  override def getSubCurve(ystart: Double, yend: Double, dir: Int): Curve = if (ystart == this.y0 && yend == this.y1) this.getWithDirection(dir)
  else if (this.x0 == this.x1) new Order1(this.x0, ystart, this.x1, yend, dir)
  else {
    val num = this.x0 - this.x1
    val denom = this.y0 - this.y1
    val xstart = this.x0 + (ystart - this.y0) * num / denom
    val xend = this.x0 + (yend - this.y0) * num / denom
    new Order1(xstart, ystart, xend, yend, dir)
  }

  override def getReversedCurve = new Order1(this.x0, this.y0, this.x1, this.y1, -this.direction)

  override def compareTo(other: Curve, yrange: Array[Double]): Int = if (!other.isInstanceOf[Order1]) super.compareTo(other, yrange)
  else {
    val c1 = other.asInstanceOf[Order1]
    if (yrange(1) <= yrange(0)) throw new InternalError("yrange already screwed up...")
    else {
      yrange(1) = Math.min(Math.min(yrange(1), this.y1), c1.y1)
      if (yrange(1) <= yrange(0)) throw new InternalError("backstepping from " + yrange(0) + " to " + yrange(1))
      else if (this.xmax <= c1.xmin) if (this.xmin == c1.xmax) 0
      else -1
      else if (this.xmin >= c1.xmax) 1
      else {
        val dxa = this.x1 - this.x0
        val dya = this.y1 - this.y0
        val dxb = c1.x1 - c1.x0
        val dyb = c1.y1 - c1.y0
        val denom = dxb * dya - dxa * dyb
        var y = .0
        if (denom != 0.0D) {
          val num = (this.x0 - c1.x0) * dya * dyb - this.y0 * dxa * dyb + c1.y0 * dxb * dya
          y = num / denom
          if (y <= yrange(0)) y = Math.min(this.y1, c1.y1)
          else {
            if (y < yrange(1)) yrange(1) = y
            y = Math.max(this.y0, c1.y0)
          }
        }
        else y = Math.max(this.y0, c1.y0)
        Curve.orderof(this.XforY(y), c1.XforY(y))
      }
    }
  }

  override def getSegment(coords: Array[Double]): Int = {
    if (this.direction == 1) {
      coords(0) = this.x1
      coords(1) = this.y1
    }
    else {
      coords(0) = this.x0
      coords(1) = this.y0
    }
    1
  }
}
