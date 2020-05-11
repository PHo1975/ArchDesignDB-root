package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//



import scala.collection.mutable.ArrayBuffer


object Order3 {
  def insert(curves: ArrayBuffer[Curve], tmp: Array[Double], x0: Double, y0: Double, cx0: Double, cy0: Double, cx1: Double, cy1: Double, x1: Double, y1: Double, direction: Int): Unit = {
    var numparams = getHorizontalParams(y0, cy0, cy1, y1, tmp)
    if (numparams == 0) addInstance(curves, x0, y0, cx0, cy0, cx1, cy1, x1, y1, direction)
    else {
      tmp(3) = x0
      tmp(4) = y0
      tmp(5) = cx0
      tmp(6) = cy0
      tmp(7) = cx1
      tmp(8) = cy1
      tmp(9) = x1
      tmp(10) = y1
      var t = tmp(0)
      if (numparams > 1 && t > tmp(1)) {
        tmp(0) = tmp(1)
        tmp(1) = t
        t = tmp(0)
      }
      split(tmp, 3, t)
      if (numparams > 1) {
        t = (tmp(1) - t) / (1.0D - t)
        split(tmp, 9, t)
      }
      var index = 3
      if (direction == -1) index += numparams * 6
      while ( {
        numparams >= 0
      }) {
        addInstance(curves, tmp(index + 0), tmp(index + 1), tmp(index + 2), tmp(index + 3), tmp(index + 4), tmp(index + 5), tmp(index + 6), tmp(index + 7), direction)
        numparams -= 1
        if (direction == 1) index += 6
        else index -= 6
      }
    }
  }

  def addInstance(curves: ArrayBuffer[Curve], x0: Double, y0: Double, cx0: Double, cy0: Double, cx1: Double, cy1: Double, x1: Double, y1: Double, direction: Int): Unit = {
    if (y0 > y1) curves.addOne(new Order3(x1, y1, cx1, cy1, cx0, cy0, x0, y0, -direction))
    else if (y1 > y0) curves.addOne(new Order3(x0, y0, cx0, cy0, cx1, cy1, x1, y1, direction))
  }

  def getHorizontalParams(c0: Double, ncp0: Double, ncp1: Double, nc1: Double, ret: Array[Double]): Int = if (c0 <= ncp0 && ncp0 <= ncp1 && ncp1 <= nc1) 0
  else {
    val c1 = nc1- ncp1
    val cp1= ncp1 - ncp0
    val cp0 = ncp0 - c0
    ret(0) = cp0
    ret(1) = (cp1 - cp0) * 2.0D
    ret(2) = c1 - cp1 - cp1 + cp0
    val numroots = QuadCurve2D.solveQuadratic(ret, ret)
    var j = 0
    for (i <- 0 until numroots) {
      val t = ret(i)
      if (t > 0.0D && t < 1.0D) {
        if (j < i) ret(j) = t
        j += 1
      }
    }
    j
  }

  def split(coords: Array[Double], pos: Int, t: Double): Unit = {
    var x1 = .0
    coords(pos + 12) = coords(pos + 6)
    x1 = coords(pos + 6)
    var y1 = .0
    coords(pos + 13) = coords(pos + 7)
      y1 = coords(pos + 7)
    var cx1 = coords(pos + 4)
    var cy1 = coords(pos + 5)
    x1 = cx1 + (x1 - cx1) * t
    y1 = cy1 + (y1 - cy1) * t
    var x0 = coords(pos + 0)
    var y0 = coords(pos + 1)
    var cx0 = coords(pos + 2)
    var cy0 = coords(pos + 3)
    x0 += (cx0 - x0) * t
    y0 += (cy0 - y0) * t
    cx0 += (cx1 - cx0) * t
    cy0 += (cy1 - cy0) * t
    cx1 = cx0 + (x1 - cx0) * t
    cy1 = cy0 + (y1 - cy0) * t
    cx0 = x0 + (cx0 - x0) * t
    cy0 = y0 + (cy0 - y0) * t
    coords(pos + 2) = x0
    coords(pos + 3) = y0
    coords(pos + 4) = cx0
    coords(pos + 5) = cy0
    coords(pos + 6) = cx0 + (cx1 - cx0) * t
    coords(pos + 7) = cy0 + (cy1 - cy0) * t
    coords(pos + 8) = cx1
    coords(pos + 9) = cy1
    coords(pos + 10) = x1
    coords(pos + 11) = y1
  }
}

final class Order3(var x0: Double, var y0: Double, var cx0: Double, var cy0: Double, var cx1: Double, var cy1: Double, var x1: Double, var y1: Double, ndirection: Int) extends Curve(ndirection) {
  private var xmin = .0
  private var xmax = .0
  private var xcoeff0 = .0
  private var xcoeff1 = .0
  private var xcoeff2 = .0
  private var xcoeff3 = .0
  private var ycoeff0 = .0
  private var ycoeff1 = .0
  private var ycoeff2 = .0
  private var ycoeff3 = .0
  private var TforY1 = .0
  private var YforT1 = .0
  private var TforY2 = .0
  private var YforT2 = .0
  private var TforY3 = .0
  private var YforT3 = .0

  if (cy0 < ycoeff0) cy0 = ycoeff0
  if (cy1 > y1) cy1 = y1
  this.xmin = Math.min(Math.min(xcoeff0, x1), Math.min(cx0, cx1))
  this.xmax = Math.max(Math.max(xcoeff0, x1), Math.max(cx0, cx1))
  this.xcoeff1 = (cx0 - xcoeff0) * 3.0D
  this.xcoeff2 = (cx1 - cx0 - cx0 + xcoeff0) * 3.0D
  this.xcoeff3 = x1 - (cx1 - cx0) * 3.0D - xcoeff0
  this.ycoeff1 = (cy0 - ycoeff0) * 3.0D
  this.ycoeff2 = (cy1 - cy0 - cy0 + ycoeff0) * 3.0D
  this.ycoeff3 = y1 - (cy1 - cy0) * 3.0D - ycoeff0
  this.YforT1 =ycoeff0
  this.YforT2 = ycoeff0
  this.YforT3 = ycoeff0

  override def getOrder = 3

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

  def getCX0: Double = if (this.direction == 1) this.cx0
  else this.cx1

  def getCY0: Double = if (this.direction == 1) this.cy0
  else this.cy1

  def getCX1: Double = if (this.direction == -1) this.cx0
  else this.cx1

  def getCY1: Double = if (this.direction == -1) this.cy0
  else this.cy1

  override def getX1: Double = if (this.direction == -1) this.x0
  else this.x1

  override def getY1: Double = if (this.direction == -1) this.y0
  else this.y1

  override def TforY(y: Double): Double = if (y <= this.y0) 0.0D
  else if (y >= this.y1) 1.0D
  else if (y == this.YforT1) this.TforY1
  else if (y == this.YforT2) this.TforY2
  else if (y == this.YforT3) this.TforY3
  else if (this.ycoeff3 == 0.0D) Order2.TforY(y, this.ycoeff0, this.ycoeff1, this.ycoeff2)
  else {
    val a = this.ycoeff2 / this.ycoeff3
    val b = this.ycoeff1 / this.ycoeff3
    val c = (this.ycoeff0 - y) / this.ycoeff3
    val roots = false
    var Q = (a * a - 3.0D * b) / 9.0D
    var R = (2.0D * a * a * a - 9.0D * a * b + 27.0D * c) / 54.0D
    val R2 = R * R
    val Q3 = Q * Q * Q
    val a_3 = a / 3.0D
    var t = .0
    var t0 = .0
    if (R2 < Q3) {
      t0 = Math.acos(R / Math.sqrt(Q3))
      Q = -2.0D * Math.sqrt(Q)
      t = this.refine(a, b, c, y, Q * Math.cos(t0 / 3.0D) - a_3)
      if (t < 0.0D) t = this.refine(a, b, c, y, Q * Math.cos((t0 + 6.283185307179586D) / 3.0D) - a_3)
      if (t < 0.0D) t = this.refine(a, b, c, y, Q * Math.cos((t0 - 6.283185307179586D) / 3.0D) - a_3)
    }
    else {
      val neg = R < 0.0D
      val S = Math.sqrt(R2 - Q3)
      if (neg) R = -R
      var A = Math.pow(R + S, 0.3333333333333333D)
      if (!neg) A = -A
      val B = if (A == 0.0D) 0.0D
      else Q / A
      t = this.refine(a, b, c, y, A + B - a_3)
    }
    def finish()={
      if (t >= 0.0D) {
        this.TforY3 = this.TforY2
        this.YforT3 = this.YforT2
        this.TforY2 = this.TforY1
        this.YforT2 = this.YforT1
        this.TforY1 = t
        this.YforT1 = y
      }
      t
    }
    if (t < 0.0D) {
      t0 = 0.0D
      var t1 = 1.0D
      while ( {
        true
      }) {
        t = (t0 + t1) / 2.0D
        if (t == t0 || t == t1) return finish()
        val yt = this.YforT(t)
        if (yt < y) t0 = t
        else {
          if (yt <= y) return finish()
          t1 = t
        }
      }
    }
    finish()
  }

  def refine(a: Double, b: Double, c: Double, target: Double, t: Double): Double = if (t >= -0.1D && t <= 1.1D) {
    var y = this.YforT(t)
    var t0 = .0
    var t1 = .0
    if (y < target) {
      t0 = t
      t1 = 1.0D
    }
    else {
      t0 = 0.0D
      t1 = t
    }
    var useslope = true
    var nt=t
    while (  y != target) {
      var t2: Double = .0
      if (!useslope) {
        t2 = (t0 + t1) / 2.0D
        if (t2 == t0 || t2 == t1) return if (nt > 1.0D) -1.0D else nt
        nt = t2
      }
      else {
        val nt2 = this.dYforT(t, 1)
        if (nt2 == 0.0D) {
          useslope = false
          //continue //todo: continue is not supported

        }
        t2 = nt + (target - y) / t2
        if (t2 == t || t2 <= t0 || t2 >= t1) {
          useslope = false
          //continue //todo: continue is not supported

        }
        nt = t2
      }
      y = this.YforT(t)
      if (y < target) t0 = nt
      else {
        if (y <= target) return if (t > 1.0D) -1.0D else nt
        t1 = nt
      }
    }
    if (nt > 1.0D) -1.0D
    else nt
  }
  else -1.0D



  override def XforY(y: Double): Double = if (y <= this.y0) this.x0
  else if (y >= this.y1) this.x1
  else this.XforT(this.TforY(y))

  override def XforT(t: Double): Double = ((this.xcoeff3 * t + this.xcoeff2) * t + this.xcoeff1) * t + this.xcoeff0

  override def YforT(t: Double): Double = ((this.ycoeff3 * t + this.ycoeff2) * t + this.ycoeff1) * t + this.ycoeff0

  override def dXforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      ((this.xcoeff3 * t + this.xcoeff2) * t + this.xcoeff1) * t + this.xcoeff0
    case 1 =>
      (3.0D * this.xcoeff3 * t + 2.0D * this.xcoeff2) * t + this.xcoeff1
    case 2 =>
      6.0D * this.xcoeff3 * t + 2.0D * this.xcoeff2
    case 3 =>
      6.0D * this.xcoeff3
    case _ =>
      0.0D
  }

  override def dYforT(t: Double, deriv: Int): Double = deriv match {
    case 0 =>
      ((this.ycoeff3 * t + this.ycoeff2) * t + this.ycoeff1) * t + this.ycoeff0
    case 1 =>
      (3.0D * this.ycoeff3 * t + 2.0D * this.ycoeff2) * t + this.ycoeff1
    case 2 =>
      6.0D * this.ycoeff3 * t + 2.0D * this.ycoeff2
    case 3 =>
      6.0D * this.ycoeff3
    case _ =>
      0.0D
  }

  override def nextVertical(t0: Double, t1: Double): Double = {
    val eqn = Array[Double](this.xcoeff1, 2.0D * this.xcoeff2, 3.0D * this.xcoeff3)
    val numroots = QuadCurve2D.solveQuadratic(eqn, eqn)
    var nt1=t1
    for (i <- 0 until numroots) {
      if (eqn(i) > t0 && eqn(i) < nt1) nt1 = eqn(i)
    }
    nt1
  }

  /*override def enlarge(r: Rectangle2D): Unit = {
    r.add(this.x0, this.y0)
    val eqn = Array[Double](this.xcoeff1, 2.0D * this.xcoeff2, 3.0D * this.xcoeff3)
    val numroots = QuadCurve2D.solveQuadratic(eqn, eqn)
    for (i <- 0 until numroots) {
      val t = eqn(i)
      if (t > 0.0D && t < 1.0D) r.add(this.XforT(t), this.YforT(t))
    }
    r.add(this.x1, this.y1)
  }*/

  override def getSubCurve(ystart: Double, yend: Double, dir: Int): Curve = if (ystart <= this.y0 && yend >= this.y1) this.getWithDirection(dir)
  else {
    val eqn = new Array[Double](14)
    var t0 = this.TforY(ystart)
    var t1 = this.TforY(yend)
    eqn(0) = this.x0
    eqn(1) = this.y0
    eqn(2) = this.cx0
    eqn(3) = this.cy0
    eqn(4) = this.cx1
    eqn(5) = this.cy1
    eqn(6) = this.x1
    eqn(7) = this.y1
    if (t0 > t1) {
      val t = t0
      t0 = t1
      t1 = t
    }
    if (t1 < 1.0D) Order3.split(eqn, 0, t1)
    var i = 0
    if (t0 <= 0.0D) i = 0
    else {
      Order3.split(eqn, 0, t0 / t1)
      i = 6
    }
    new Order3(eqn(i + 0), ystart, eqn(i + 2), eqn(i + 3), eqn(i + 4), eqn(i + 5), eqn(i + 6), yend, dir)
  }

  override def getReversedCurve = new Order3(this.x0, this.y0, this.cx0, this.cy0, this.cx1, this.cy1, this.x1, this.y1, -this.direction)

  override def getSegment(coords: Array[Double]): Int = {
    if (this.direction == 1) {
      coords(0) = this.cx0
      coords(1) = this.cy0
      coords(2) = this.cx1
      coords(3) = this.cy1
      coords(4) = this.x1
      coords(5) = this.y1
    }
    else {
      coords(0) = this.cx1
      coords(1) = this.cy1
      coords(2) = this.cx0
      coords(3) = this.cy0
      coords(4) = this.x0
      coords(5) = this.y0
    }
    3
  }

  override def controlPointString: String = {
    val var10000 = Curve.round(this.getCX0)
    "(" + var10000 + ", " + Curve.round(this.getCY0) + "), (" + Curve.round(this.getCX1) + ", " + Curve.round(this.getCY1) + "), "
  }
}
