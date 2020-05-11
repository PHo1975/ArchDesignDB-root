package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


import scala.collection.mutable.ArrayBuffer


object Order2 {
  def insert(curves: ArrayBuffer[Curve], tmp: Array[Double], x0: Double, y0: Double, cx0: Double, cy0: Double, x1: Double, y1: Double, direction: Int): Unit = {
    val numparams = getHorizontalParams(y0, cy0, y1, tmp)
    if (numparams == 0) addInstance(curves, x0, y0, cx0, cy0, x1, y1, direction)
    else {
      val t = tmp(0)
      tmp(0) = x0
      tmp(1) = y0
      tmp(2) = cx0
      tmp(3) = cy0
      tmp(4) = x1
      tmp(5) = y1
      split(tmp, 0, t)
      val i0 = if (direction == 1) 0
      else 4
      val i1 = 4 - i0
      addInstance(curves, tmp(i0), tmp(i0 + 1), tmp(i0 + 2), tmp(i0 + 3), tmp(i0 + 4), tmp(i0 + 5), direction)
      addInstance(curves, tmp(i1), tmp(i1 + 1), tmp(i1 + 2), tmp(i1 + 3), tmp(i1 + 4), tmp(i1 + 5), direction)
    }
  }

  def addInstance(curves: ArrayBuffer[Curve], x0: Double, y0: Double, cx0: Double, cy0: Double, x1: Double, y1: Double, direction: Int): Unit = {
    if (y0 > y1) curves.addOne(new Order2(x1, y1, cx0, cy0, x0, y0, -direction))
    else if (y1 > y0) curves.addOne(new Order2(x0, y0, cx0, cy0, x1, y1, direction))
  }

  def getHorizontalParams(c0: Double, cp: Double, c1: Double, ret: Array[Double]): Int = if (c0 <= cp && cp <= c1) 0
  else {
    val nc0 = c0 - cp
    val nc1 = c1 - cp
    val denom = nc0 + nc1
    if (denom == 0.0D) 0
    else {
      val t = nc0 / denom
      if (t > 0.0D && t < 1.0D) {
        ret(0) = t
        1
      }
      else 0
    }
  }

  def split(coords: Array[Double], pos: Int, t: Double): Unit = {
    var x1 = .0
    coords(pos + 8) = coords(pos + 4)
    x1 = coords(pos + 4)
    var y1 = .0
    coords(pos + 9) = coords(pos + 5)
    y1 = coords(pos + 5)
    var cx = coords(pos + 2)
    var cy = coords(pos + 3)
    x1 = cx + (x1 - cx) * t
    y1 = cy + (y1 - cy) * t
    var x0 = coords(pos + 0)
    var y0 = coords(pos + 1)
    x0 += (cx - x0) * t
    y0 += (cy - y0) * t
    cx = x0 + (x1 - x0) * t
    cy = y0 + (y1 - y0) * t
    coords(pos + 2) = x0
    coords(pos + 3) = y0
    coords(pos + 4) = cx
    coords(pos + 5) = cy
    coords(pos + 6) = x1
    coords(pos + 7) = y1
  }

  def TforY(y: Double, nycoeff0: Double, ycoeff1: Double, ycoeff2: Double): Double = {
    val ycoeff0= nycoeff0 - y
    var d = .0
    var q = .0
    if (ycoeff2 == 0.0D) {
      d = -ycoeff0 / ycoeff1
      if (d >= 0.0D && d <= 1.0D) return d
    }
    else {
      d = ycoeff1 * ycoeff1 - 4.0D * ycoeff2 * ycoeff0
      if (d >= 0.0D) {
        d = Math.sqrt(d)
        if (ycoeff1 < 0.0D) d = -d
        q = (ycoeff1 + d) / -2.0D
        var root = q / ycoeff2
        if (root >= 0.0D && root <= 1.0D) return root
        if (q != 0.0D) {
          root = ycoeff0 / q
          if (root >= 0.0D && root <= 1.0D) return root
        }
      }
    }
    q = ycoeff0 + ycoeff1 + ycoeff2
    if (0.0D < (ycoeff0 + q) / 2.0D) 0.0D
    else 1.0D
  }
}

final class Order2(var x0: Double, var y0: Double, var cx0: Double, var cy0: Double, var x1: Double, var y1: Double, ndirection: Int) extends Curve(ndirection) {
  private var xmin = .0
  private var xmax = .0
  private var xcoeff0 = .0
  private var xcoeff1 = .0
  private var xcoeff2 = .0
  private var ycoeff0 = .0
  private var ycoeff1 = .0
  private var ycoeff2 = .0


  if (cy0 < ycoeff0) cy0 = ycoeff0
  else if (cy0 > y1) cy0 = y1
  this.xmin = Math.min(Math.min(xcoeff0, x1), cx0)
  this.xmax = Math.max(Math.max(xcoeff0, x1), cx0)
  this.xcoeff1 = cx0 + cx0 - xcoeff0 - xcoeff0
  this.xcoeff2 = xcoeff0 - cx0 - cx0 + x1
  this.ycoeff1 = cy0 + cy0 - ycoeff0 - ycoeff0
  this.ycoeff2 = ycoeff0 - cy0 - cy0 + y1

  override def getOrder = 2

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

  def getCX0: Double = this.cx0

  def getCY0: Double = this.cy0

  override def getX1: Double = if (this.direction == -1) this.x0
  else this.x1

  override def getY1: Double = if (this.direction == -1) this.y0
  else this.y1

  override def XforY(y: Double): Double = if (y <= this.y0) this.x0
  else if (y >= this.y1) this.x1
  else this.XforT(this.TforY(y))

  override def TforY(y: Double): Double = if (y <= this.y0) 0.0D
  else if (y >= this.y1) 1.0D
  else Order2.TforY(y, this.ycoeff0, this.ycoeff1, this.ycoeff2)

  override def XforT(t: Double): Double = (this.xcoeff2 * t + this.xcoeff1) * t + this.xcoeff0

  override def YforT(t: Double): Double = (this.ycoeff2 * t + this.ycoeff1) * t + this.ycoeff0

  override def dXforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      (this.xcoeff2 * t + this.xcoeff1) * t + this.xcoeff0
    case 1 =>
      2.0D * this.xcoeff2 * t + this.xcoeff1
    case 2 =>
      2.0D * this.xcoeff2
    case _ =>
      0.0D
  }

  override def dYforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      (this.ycoeff2 * t + this.ycoeff1) * t + this.ycoeff0
    case 1 =>
      2.0D * this.ycoeff2 * t + this.ycoeff1
    case 2 =>
      2.0D * this.ycoeff2
    case _ =>
      0.0D
  }

  override def nextVertical(t0: Double, t1: Double): Double = {
    val t = -this.xcoeff1 / (2.0D * this.xcoeff2)
    if (t > t0 && t < t1) t
    else t1
  }

  /*override def enlarge(r: Rectangle2D): Unit = {
    r.add(this.x0, this.y0)
    val t = -this.xcoeff1 / (2.0D * this.xcoeff2)
    if (t > 0.0D && t < 1.0D) r.add(this.XforT(t), this.YforT(t))
    r.add(this.x1, this.y1)
  }*/

  override def getSubCurve(ystart: Double, yend: Double, dir: Int): Curve = {
    var t0 = .0
    if (ystart <= this.y0) {
      if (yend >= this.y1) return this.getWithDirection(dir)
      t0 = 0.0D
    }
    else t0 = Order2.TforY(ystart, this.ycoeff0, this.ycoeff1, this.ycoeff2)
    var t1 = .0
    if (yend >= this.y1) t1 = 1.0D
    else t1 = Order2.TforY(yend, this.ycoeff0, this.ycoeff1, this.ycoeff2)
    val eqn = new Array[Double](10)
    eqn(0) = this.x0
    eqn(1) = this.y0
    eqn(2) = this.cx0
    eqn(3) = this.cy0
    eqn(4) = this.x1
    eqn(5) = this.y1
    if (t1 < 1.0D) Order2.split(eqn, 0, t1)
    var i = 0
    if (t0 <= 0.0D) i = 0
    else {
      Order2.split(eqn, 0, t0 / t1)
      i = 4
    }
    new Order2(eqn(i + 0), ystart, eqn(i + 2), eqn(i + 3), eqn(i + 4), yend, dir)
  }

  override def getReversedCurve = new Order2(this.x0, this.y0, this.cx0, this.cy0, this.x1, this.y1, -this.direction)

  override def getSegment(coords: Array[Double]): Int = {
    coords(0) = this.cx0
    coords(1) = this.cy0
    if (this.direction == 1) {
      coords(2) = this.x1
      coords(3) = this.y1
    }
    else {
      coords(2) = this.x0
      coords(3) = this.y0
    }
    2
  }

  override def controlPointString: String = {
    val var10000 = Curve.round(this.cx0)
    "(" + var10000 + ", " + Curve.round(this.cy0) + "), "
  }
}
