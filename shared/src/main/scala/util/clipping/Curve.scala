package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//




import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(-5158084205220481094L)
class IllegalPathStateException(s:String) extends RuntimeException(s)


object Curve {
  val INCREASING = 1
  val DECREASING: Int = -1
  val RECT_INTERSECTS: Int = -2147483648
  val TMIN = 0.001D

  def insertMove(curves: ArrayBuffer[Curve], x: Double, y: Double): Unit = {
    curves.addOne(new Order0(x, y))
  }

  def insertLine(curves: ArrayBuffer[Curve], x0: Double, y0: Double, x1: Double, y1: Double): Unit = {
    if (y0 < y1) curves.addOne(new Order1(x0, y0, x1, y1, 1))
    else if (y0 > y1) curves.addOne(new Order1(x1, y1, x0, y0, -1))
  }

  def insertQuad(curves: ArrayBuffer[Curve], x0: Double, y0: Double, coords: Array[Double]): Unit = {
    val y1 = coords(3)
    if (y0 > y1) Order2.insert(curves, coords, coords(2), y1, coords(0), coords(1), x0, y0, -1)
    else {
      if (y0 == y1 && y0 == coords(1)) return
      Order2.insert(curves, coords, x0, y0, coords(0), coords(1), coords(2), y1, 1)
    }
  }

  def insertCubic(curves: ArrayBuffer[Curve], x0: Double, y0: Double, coords: Array[Double]): Unit = {
    val y1 = coords(5)
    if (y0 > y1) Order3.insert(curves, coords, coords(4), y1, coords(2), coords(3), coords(0), coords(1), x0, y0, -1)
    else {
      if (y0 == y1 && y0 == coords(1) && y0 == coords(3)) return
      Order3.insert(curves, coords, x0, y0, coords(0), coords(1), coords(2), coords(3), coords(4), y1, 1)
    }
  }

  def pointCrossingsForPath(pi: PathIterator, px: Double, py: Double): Int = if (pi.isDone) 0
  else {
    val coords = new Array[Double](6)
    if (pi.currentSegment(coords) != 0) throw new IllegalPathStateException("missing initial moveto in path definition")
    else {
      pi.next()
      var movx = coords(0)
      var movy = coords(1)
      var curx = movx
      var cury = movy
      var crossings = 0
      crossings = 0
      while ( {
        !pi.isDone
      }) {
        var endy = .0
        var endx = .0
        pi.currentSegment(coords) match {
          case 0 =>
            if (cury != movy) crossings += pointCrossingsForLine(px, py, curx, cury, movx, movy)
            movx = coords(0)
            curx = coords(0)
            movy = coords(1)
            cury = coords(1)

          case 1 =>
            endx = coords(0)
            endy = coords(1)
            crossings += pointCrossingsForLine(px, py, curx, cury, endx, endy)
            curx = endx
            cury = endy

          case 2 =>
            endx = coords(2)
            endy = coords(3)
            crossings += pointCrossingsForQuad(px, py, curx, cury, coords(0), coords(1), endx, endy, 0)
            curx = endx
            cury = endy

          case 3 =>
            endx = coords(4)
            endy = coords(5)
            crossings += pointCrossingsForCubic(px, py, curx, cury, coords(0), coords(1), coords(2), coords(3), endx, endy, 0)
            curx = endx
            cury = endy

          case 4 =>
            if (cury != movy) crossings += pointCrossingsForLine(px, py, curx, cury, movx, movy)
            curx = movx
            cury = movy
        }

        pi.next()
      }
      if (cury != movy) crossings += pointCrossingsForLine(px, py, curx, cury, movx, movy)
      crossings
    }
  }

  def pointCrossingsForLine(px: Double, py: Double, x0: Double, y0: Double, x1: Double, y1: Double): Int = if (py < y0 && py < y1) 0
  else if (py >= y0 && py >= y1) 0
  else if (px >= x0 && px >= x1) 0
  else if (px < x0 && px < x1) if (y0 < y1) 1
  else -1
  else {
    val xintercept = x0 + (py - y0) * (x1 - x0) / (y1 - y0)
    if (px >= xintercept) 0
    else if (y0 < y1) 1
    else -1
  }

  def pointCrossingsForQuad(px: Double, py: Double, x0: Double, y0: Double, nxc: Double, nyc: Double, x1: Double, y1: Double, level: Int): Int = if (py < y0 && py < nyc && py < y1) 0
  else if (py >= y0 && py >= nyc && py >= y1) 0
  else if (px >= x0 && px >= nxc && px >= x1) 0
  else if (px < x0 && px < nxc && px < x1) {
    if (py >= y0) if (py < y1) return 1
    else if (py >= y1) return -1
    0
  }
  else if (level > 52) pointCrossingsForLine(px, py, x0, y0, x1, y1)
  else {
    val x0c = (x0 + nxc) / 2.0D
    val y0c = (y0 + nyc) / 2.0D
    val xc1 = (nxc + x1) / 2.0D
    val yc1 = (nyc + y1) / 2.0D
    val xc: Double = (x0c + xc1) / 2.0D
    val yc = (y0c + yc1) / 2.0D
    if (!java.lang.Double.isNaN(xc) && !java.lang.Double.isNaN(yc)) pointCrossingsForQuad(px, py, x0, y0, x0c, y0c, xc, yc, level + 1) + pointCrossingsForQuad(px, py, xc, yc, xc1, yc1, x1, y1, level + 1)
    else 0
  }

  def pointCrossingsForCubic(px: Double, py: Double, x0: Double, y0: Double, xc0: Double, yc0: Double, xc1: Double, yc1: Double, x1: Double, y1: Double, level: Int): Int = if (py < y0 && py < yc0 && py < yc1 && py < y1) 0
  else if (py >= y0 && py >= yc0 && py >= yc1 && py >= y1) 0
  else if (px >= x0 && px >= xc0 && px >= xc1 && px >= x1) 0
  else if (px < x0 && px < xc0 && px < xc1 && px < x1) {
    if (py >= y0) if (py < y1) return 1
    else if (py >= y1) return -1
    0
  }
  else if (level > 52) pointCrossingsForLine(px, py, x0, y0, x1, y1)
  else {
    var xmid = (xc0 + xc1) / 2.0D
    var ymid = (yc0 + yc1) / 2.0D
    val nxc0 = (x0 + xc0) / 2.0D
    val nyc0 = (y0 + yc0) / 2.0D
    val nxc1 = (xc1 + x1) / 2.0D
    val nyc1 = (yc1 + y1) / 2.0D
    val xc0m = (nxc0 + xmid) / 2.0D
    val yc0m = (nyc0 + ymid) / 2.0D
    val xmc1 = (xmid + nxc1) / 2.0D
    val ymc1 = (ymid + nyc1) / 2.0D
    xmid = (xc0m + xmc1) / 2.0D
    ymid = (yc0m + ymc1) / 2.0D
    if (!java.lang.Double.isNaN(xmid) && !java.lang.Double.isNaN(ymid)) pointCrossingsForCubic(px, py, x0, y0, nxc0, nyc0, xc0m, yc0m, xmid, ymid, level + 1) + pointCrossingsForCubic(px, py, xmid, ymid, xmc1, ymc1, nxc1, nyc1, x1, y1, level + 1)
    else 0
  }

  def rectCrossingsForPath(pi: PathIterator, rxmin: Double, rymin: Double, rxmax: Double, rymax: Double): Int = if (rxmax > rxmin && rymax > rymin) if (pi.isDone) 0
  else {
    val coords = new Array[Double](6)
    if (pi.currentSegment(coords) != 0) throw new IllegalPathStateException("missing initial moveto in path definition")
    else {
      pi.next()
      var movx = coords(0)
      var curx = coords(0)
      var movy = coords(1)
      var cury = coords(1)
      var crossings = 0
      crossings = 0
      while ( {
        crossings != -2147483648 && !pi.isDone
      }) {
        var endx = .0
        var endy = .0
        pi.currentSegment(coords) match {
          case 0 =>
            if (curx != movx || cury != movy) crossings = rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
            movx = coords(0)
              curx = coords(0)
            movy = coords(1)
              cury = coords(1)

          case 1 =>
            endx = coords(0)
            endy = coords(1)
            crossings = rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, endx, endy)
            curx = endx
            cury = endy

          case 2 =>
            endx = coords(2)
            endy = coords(3)
            crossings = rectCrossingsForQuad(crossings, rxmin, rymin, rxmax, rymax, curx, cury, coords(0), coords(1), endx, endy, 0)
            curx = endx
            cury = endy

          case 3 =>
            endx = coords(4)
            endy = coords(5)
            crossings = rectCrossingsForCubic(crossings, rxmin, rymin, rxmax, rymax, curx, cury, coords(0), coords(1), coords(2), coords(3), endx, endy, 0)
            curx = endx
            cury = endy

          case 4 =>
            if (curx != movx || cury != movy) crossings = rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
            curx = movx
            cury = movy
        }

        pi.next()
      }
      if (crossings != -2147483648 && (curx != movx || cury != movy)) crossings = rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
      crossings
    }
  }
  else 0

  def rectCrossingsForLine(crossings: Int, rxmin: Double, rymin: Double, rxmax: Double, rymax: Double, x0: Double, y0: Double, x1: Double, y1: Double): Int = if (y0 >= rymax && y1 >= rymax) crossings
  else if (y0 <= rymin && y1 <= rymin) crossings
  else if (x0 <= rxmin && x1 <= rxmin) crossings
  else if (x0 >= rxmax && x1 >= rxmax) {
    var ncrossings=crossings
    if (y0 < y1) {
      if (y0 <= rymin) ncrossings += 1
      if (y1 >= rymax) ncrossings += 1
    }
    else if (y1 < y0) {
      if (y1 <= rymin) ncrossings -= 1
      if (y0 >= rymax) ncrossings -= 1
    }
    ncrossings
  }
  else if (x0 > rxmin && x0 < rxmax && y0 > rymin && y0 < rymax || x1 > rxmin && x1 < rxmax && y1 > rymin && y1 < rymax) -2147483648
  else {
    var xi0 = x0
    if (y0 < rymin) xi0 = x0 + (rymin - y0) * (x1 - x0) / (y1 - y0)
    else if (y0 > rymax) xi0 = x0 + (rymax - y0) * (x1 - x0) / (y1 - y0)
    var xi1 = x1
    if (y1 < rymin) xi1 = x1 + (rymin - y1) * (x0 - x1) / (y0 - y1)
    else if (y1 > rymax) xi1 = x1 + (rymax - y1) * (x0 - x1) / (y0 - y1)
    if (xi0 <= rxmin && xi1 <= rxmin) crossings
    else if (xi0 >= rxmax && xi1 >= rxmax) {
      var ncrossings=crossings
      if (y0 < y1) {
        if (y0 <= rymin) ncrossings += 1
        if (y1 >= rymax) ncrossings += 1
      }
      else if (y1 < y0) {
        if (y1 <= rymin) ncrossings -= 1
        if (y0 >= rymax) ncrossings -= 1
      }
      ncrossings
    }
    else -2147483648
  }

  def rectCrossingsForQuad(crossings: Int, rxmin: Double, rymin: Double, rxmax: Double, rymax: Double, x0: Double, y0: Double, xc: Double, yc: Double, x1: Double, y1: Double, level: Int): Int = if (y0 >= rymax && yc >= rymax && y1 >= rymax) crossings
  else if (y0 <= rymin && yc <= rymin && y1 <= rymin) crossings
  else if (x0 <= rxmin && xc <= rxmin && x1 <= rxmin) crossings
  else if (x0 >= rxmax && xc >= rxmax && x1 >= rxmax) {
    var ncrossings=crossings
    if (y0 < y1) {
      if (y0 <= rymin && y1 > rymin) ncrossings += 1
      if (y0 < rymax && y1 >= rymax) ncrossings += 1
    }
    else if (y1 < y0) {
      if (y1 <= rymin && y0 > rymin) ncrossings -= 1
      if (y1 < rymax && y0 >= rymax) ncrossings -= 1
    }
    ncrossings
  }
  else if (x0 < rxmax && x0 > rxmin && y0 < rymax && y0 > rymin || x1 < rxmax && x1 > rxmin && y1 < rymax && y1 > rymin) -2147483648
  else if (level > 52) rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, x0, y0, x1, y1)
  else {
    val x0c = (x0 + xc) / 2.0D
    val y0c = (y0 + yc) / 2.0D
    val xc1 = (xc + x1) / 2.0D
    val yc1 = (yc + y1) / 2.0D
    val nxc = (x0c + xc1) / 2.0D
    val nyc = (y0c + yc1) / 2.0D
    if (!java.lang.Double.isNaN(nxc) && !java.lang.Double.isNaN(nyc)) {
      var ncrossings = rectCrossingsForQuad(crossings, rxmin, rymin, rxmax, rymax, x0, y0, x0c, y0c, nxc, nyc, level + 1)
      if (ncrossings != -2147483648) ncrossings = rectCrossingsForQuad(crossings, rxmin, rymin, rxmax, rymax, nxc, nyc, xc1, yc1, x1, y1, level + 1)
      ncrossings
    }
    else 0
  }

  def rectCrossingsForCubic(crossings: Int, rxmin: Double, rymin: Double, rxmax: Double, rymax: Double, x0: Double, y0: Double, xc0: Double, yc0: Double, xc1: Double, yc1: Double, x1: Double, y1: Double, level: Int): Int = if (y0 >= rymax && yc0 >= rymax && yc1 >= rymax && y1 >= rymax) crossings
  else if (y0 <= rymin && yc0 <= rymin && yc1 <= rymin && y1 <= rymin) crossings
  else if (x0 <= rxmin && xc0 <= rxmin && xc1 <= rxmin && x1 <= rxmin) crossings
  else if (x0 >= rxmax && xc0 >= rxmax && xc1 >= rxmax && x1 >= rxmax) {
    var ncrossings=crossings
    if (y0 < y1) {
      if (y0 <= rymin && y1 > rymin) ncrossings += 1
      if (y0 < rymax && y1 >= rymax) ncrossings += 1
    }
    else if (y1 < y0) {
      if (y1 <= rymin && y0 > rymin) ncrossings -= 1
      if (y1 < rymax && y0 >= rymax) ncrossings -= 1
    }
    ncrossings
  }
  else if (x0 > rxmin && x0 < rxmax && y0 > rymin && y0 < rymax || x1 > rxmin && x1 < rxmax && y1 > rymin && y1 < rymax) -2147483648
  else if (level > 52) rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, x0, y0, x1, y1)
  else {
    var xmid = (xc0 + xc1) / 2.0D
    var ymid = (yc0 + yc1) / 2.0D
    val nxc0 = (x0 + xc0) / 2.0D
    val nyc0 = (y0 + yc0) / 2.0D
    val nxc1 = (xc1 + x1) / 2.0D
    val nyc1 = (yc1 + y1) / 2.0D
    val xc0m = (nxc0 + xmid) / 2.0D
    val yc0m = (nyc0 + ymid) / 2.0D
    val xmc1 = (xmid + nxc1) / 2.0D
    val ymc1 = (ymid + nyc1) / 2.0D
    xmid = (xc0m + xmc1) / 2.0D
    ymid = (yc0m + ymc1) / 2.0D
    if (!java.lang.Double.isNaN(xmid) && !java.lang.Double.isNaN(ymid)) {
      var ncrossings = rectCrossingsForCubic(crossings, rxmin, rymin, rxmax, rymax, x0, y0, nxc0, nyc0, xc0m, yc0m, xmid, ymid, level + 1)
      if (ncrossings != -2147483648) ncrossings = rectCrossingsForCubic(crossings, rxmin, rymin, rxmax, rymax, xmid, ymid, xmc1, ymc1, nxc1, nyc1, x1, y1, level + 1)
      ncrossings
    }
    else 0
  }

  def round(v: Double): Double = v

  def orderof(x1: Double, x2: Double): Int = if (x1 < x2) -1
  else if (x1 > x2) 1
  else 0

  def signeddiffbits(y1: Double, y2: Double): Long = java.lang.Double.doubleToLongBits(y1) - java.lang.Double.doubleToLongBits(y2)

  def diffbits(y1: Double, y2: Double): Long = Math.abs(java.lang.Double.doubleToLongBits(y1) - java.lang.Double.doubleToLongBits(y2))

  def prev(v: Double): Double = java.lang.Double.longBitsToDouble(java.lang.Double.doubleToLongBits(v) - 1L)

  def next(v: Double): Double = java.lang.Double.longBitsToDouble(java.lang.Double.doubleToLongBits(v) + 1L)
}

abstract class Curve(var direction: Int) {
  final def getDirection: Int = this.direction

  final def getWithDirection(direction: Int): Curve = if (this.direction == direction) this
  else this.getReversedCurve

  override def toString: String = {
    val var10000 = this.getOrder
    "Curve[" + var10000 + ", (" + Curve.round(this.getX0) + ", " + Curve.round(this.getY0) + "), " + this.controlPointString + "(" + Curve.round(this.getX1) + ", " + Curve.round(this.getY1) + "), " + (if (this.direction == 1) "D"
    else "U") + "]"
  }

  def controlPointString = ""

  def getOrder: Int

  def getXTop: Double

  def getYTop: Double

  def getXBot: Double

  def getYBot: Double

  def getXMin: Double

  def getXMax: Double

  def getX0: Double

  def getY0: Double

  def getX1: Double

  def getY1: Double

  def XforY(var1: Double): Double

  def TforY(var1: Double): Double

  def XforT(var1: Double): Double

  def YforT(var1: Double): Double

  def dXforT(var1: Double, var3: Int): Double

  def dYforT(var1: Double, var3: Int): Double

  def nextVertical(var1: Double, var3: Double): Double

  def crossingsFor(x: Double, y: Double): Int = if (y < this.getYTop || y >= this.getYBot || x >= this.getXMax || x >= this.getXMin && x >= this.XforY(y)) 0
  else 1

  def accumulateCrossings(c: Crossings): Boolean = {
    val xhi = c.getXHi
    if (this.getXMin >= xhi) false
    else {
      val xlo = c.getXLo
      val ylo = c.getYLo
      val yhi = c.getYHi
      val y0 = this.getYTop
      val y1 = this.getYBot
      var tstart = .0
      var ystart = .0
      if (y0 < ylo) {
        if (y1 <= ylo) return false
        ystart = ylo
        tstart = this.TforY(ylo)
      }
      else {
        if (y0 >= yhi) return false
        ystart = y0
        tstart = 0.0D
      }
      var tend = .0
      var yend = .0
      if (y1 > yhi) {
        yend = yhi
        tend = this.TforY(yhi)
      }
      else {
        yend = y1
        tend = 1.0D
      }
      var hitLo = false
      var hitHi = false
      while ( true  ) {
        val x = this.XforT(tstart)
        if (x < xhi) {
          if (hitHi || x > xlo) return true
          hitLo = true
        }
        else {
          if (hitLo) return true
          hitHi = true
        }
        if (tstart >= tend) {
          if (hitLo) c.record(ystart, yend, this.direction)
          return false
        }
        tstart = this.nextVertical(tstart, tend)
      }
    }
    false
  }

  //def enlarge(var1: Rectangle2D): Unit

  def getSubCurve(ystart: Double, yend: Double): Curve = this.getSubCurve(ystart, yend, this.direction)

  def getReversedCurve: Curve

  def getSubCurve(var1: Double, var3: Double, var5: Int): Curve

  def compareTo(that: Curve, yrange: Array[Double]): Int = {
    val y0 = yrange(0)
    var y1 = yrange(1)
    y1 = Math.min(Math.min(y1, this.getYBot), that.getYBot)
    if (y1 <= yrange(0)) {
      System.err.println("this == " + this)
      System.err.println("that == " + that)
      System.out.println("target range = " + yrange(0) + "=>" + yrange(1))
      throw new InternalError("backstepping from " + yrange(0) + " to " + y1)
    }
    else {
      yrange(1) = y1
      if (this.getXMax <= that.getXMin) if (this.getXMin == that.getXMax) 0
      else -1
      else if (this.getXMin >= that.getXMax) 1
      else {
        var s0 = this.TforY(y0)
        var ys0 = this.YforT(s0)
        if (ys0 < y0) {
          s0 = this.refineTforY(s0, ys0, y0)
          ys0 = this.YforT(s0)
        }
        var s1 = this.TforY(y1)
        if (this.YforT(s1) < y0) s1 = this.refineTforY(s1, this.YforT(s1), y0)
        var t0 = that.TforY(y0)
        var yt0 = that.YforT(t0)
        if (yt0 < y0) {
          t0 = that.refineTforY(t0, yt0, y0)
          yt0 = that.YforT(t0)
        }
        var t1 = that.TforY(y1)
        if (that.YforT(t1) < y0) t1 = that.refineTforY(t1, that.YforT(t1), y0)
        var xs0 = this.XforT(s0)
        var xt0 = that.XforT(t0)
        val scale = Math.max(Math.abs(y0), Math.abs(y1))
        val ymin = Math.max(scale * 1.0E-14D, 1.0E-300D)
        var th = .0
        var sh = .0
        var xsh = .0
        var ysh = .0
        if (this.fairlyClose(xs0, xt0)) {
          sh = ymin
          xsh = Math.min(ymin * 1.0E13D, (y1 - y0) * 0.1D)
          ysh = y0 + ymin
          var goOn:Boolean=true
          while ( ysh <= y1 && goOn ) {
            if (!this.fairlyClose(this.XforY(ysh), that.XforY(ysh))) {
              ysh -= sh
              while (goOn)  {
                sh /= 2.0D
                th = ysh + sh
                if (th <= ysh) goOn=false
                else if (this.fairlyClose(this.XforY(th), that.XforY(th))) ysh = th
              }
            }
            if(goOn) {
              sh *= 2.0D
              if (sh > xsh) sh = xsh
              ysh += sh
            }
          }
          if (ysh > y0) {
            if (ysh < y1) yrange(1) = ysh
            return 0
          }
        }
        if (ymin <= 0.0D) System.out.println("ymin = " + ymin)
        var goOn:Boolean=true
        while (   s0 < s1 && t0 < t1 && goOn ) {
          sh = this.nextVertical(s0, s1)
          xsh = this.XforT(sh)
          ysh = this.YforT(sh)
          th = that.nextVertical(t0, t1)
          val xth = that.XforT(th)
          val yth = that.YforT(th)
          try if (this.findIntersect(that, yrange, ymin, 0, 0, s0, xs0, ys0, sh, xsh, ysh, t0, xt0, yt0, th, xth, yth)) {goOn=false}
          catch {
            case var40: Throwable =>
              System.err.println("Error: " + var40)
              System.err.println("y range was " + yrange(0) + "=>" + yrange(1))
              System.err.println("s y range is " + ys0 + "=>" + ysh)
              System.err.println("t y range is " + yt0 + "=>" + yth)
              System.err.println("ymin is " + ymin)
              return 0
          }
          if(goOn) {
            if (ysh < yth) {
              if (ysh > yrange(0)) {
                if (ysh < yrange(1)) yrange(1) = ysh
                goOn=false

              }
              if(goOn) {
                s0 = sh
                xs0 = xsh
                ys0 = ysh
              }
            }
            else {
              if (yth > yrange(0)) {
                if (yth < yrange(1)) yrange(1) = yth
                goOn=false
              }
              if(goOn) {
                t0 = th
                xt0 = xth
                yt0 = yth
              }
            }
          }
        }
        sh = (yrange(0) + yrange(1)) / 2.0D
        Curve.orderof(this.XforY(sh), that.XforY(sh))
      }
    }
  }

  def findIntersect(that: Curve, yrange: Array[Double], ymin: Double, slevel: Int, tlevel: Int, s0: Double, xs0: Double, ys0: Double, s1: Double, xs1: Double, ys1: Double, t0: Double, xt0: Double, yt0: Double, t1: Double, xt1: Double, yt1: Double): Boolean = if (ys0 <= yt1 && yt0 <= ys1) if (Math.min(xs0, xs1) <= Math.max(xt0, xt1) && Math.max(xs0, xs1) >= Math.min(xt0, xt1)) {
    var s = .0
    var xs = .0
    var ys = .0
    var t = .0
    var xt = .0
    var yt = .0
    if (s1 - s0 > 0.001D) {
      s = (s0 + s1) / 2.0D
      xs = this.XforT(s)
      ys = this.YforT(s)
      if (s == s0 || s == s1) {
        System.out.println("s0 = " + s0)
        System.out.println("s1 = " + s1)
        throw new InternalError("no s progress!")
      }
      if (t1 - t0 > 0.001D) {
        t = (t0 + t1) / 2.0D
        xt = that.XforT(t)
        yt = that.YforT(t)
        if (t == t0 || t == t1) {
          System.out.println("t0 = " + t0)
          System.out.println("t1 = " + t1)
          throw new InternalError("no t progress!")
        }
        if (ys >= yt0 && yt >= ys0 && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel + 1, s0, xs0, ys0, s, xs, ys, t0, xt0, yt0, t, xt, yt)) return true
        if (ys >= yt && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel + 1, s0, xs0, ys0, s, xs, ys, t, xt, yt, t1, xt1, yt1)) return true
        if (yt >= ys && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel + 1, s, xs, ys, s1, xs1, ys1, t0, xt0, yt0, t, xt, yt)) return true
        if (ys1 >= yt && yt1 >= ys && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel + 1, s, xs, ys, s1, xs1, ys1, t, xt, yt, t1, xt1, yt1)) return true
      }
      else {
        if (ys >= yt0 && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel, s0, xs0, ys0, s, xs, ys, t0, xt0, yt0, t1, xt1, yt1)) return true
        if (yt1 >= ys && this.findIntersect(that, yrange, ymin, slevel + 1, tlevel, s, xs, ys, s1, xs1, ys1, t0, xt0, yt0, t1, xt1, yt1)) return true
      }
    }
    else if (t1 - t0 > 0.001D) {
      s = (t0 + t1) / 2.0D
      xs = that.XforT(s)
      ys = that.YforT(s)
      if (s == t0 || s == t1) {
        System.out.println("t0 = " + t0)
        System.out.println("t1 = " + t1)
        throw new InternalError("no t progress!")
      }
      if (ys >= ys0 && this.findIntersect(that, yrange, ymin, slevel, tlevel + 1, s0, xs0, ys0, s1, xs1, ys1, t0, xt0, yt0, s, xs, ys)) return true
      if (ys1 >= ys && this.findIntersect(that, yrange, ymin, slevel, tlevel + 1, s0, xs0, ys0, s1, xs1, ys1, s, xs, ys, t1, xt1, yt1)) return true
    }
    else {
      s = xs1 - xs0
      xs = ys1 - ys0
      ys = xt1 - xt0
      t = yt1 - yt0
      xt = xt0 - xs0
      yt = yt0 - ys0
      val det = ys * xs - t * s
      if (det != 0.0D) {
        val detinv = 1.0D / det
        var s = (ys * yt - t * xt) * detinv
        var nt = (s * yt - xs * xt) * detinv
        if (s >= 0.0D && s <= 1.0D && nt >= 0.0D && nt <= 1.0D) {
          s = s0 + s * (s1 - s0)
          nt = t0 + nt * (t1 - t0)
          if (s < 0.0D || s > 1.0D || nt < 0.0D || nt > 1.0D) System.out.println("Uh oh!")
          val y = (this.YforT(s) + that.YforT(nt)) / 2.0D
          if (y <= yrange(1) && y > yrange(0)) {
            yrange(1) = y
            return true
          }
        }
      }
    }
    false
  }
  else false
  else false

  def refineTforY(t0: Double, yt0: Double, y0: Double): Double = {
    var t1 = 1.0D
    var nt0=t0
    while ( true ) {
      val th = (nt0 + t1) / 2.0D
      if (th == nt0 || th == t1) return t1
      val y = this.YforT(th)
      if (y < y0) nt0 = th
      else {
        if (y <= y0) return t1
        t1 = th
      }
    }
    0d
  }

  def fairlyClose(v1: Double, v2: Double): Boolean = Math.abs(v1 - v2) < Math.max(Math.abs(v1), Math.abs(v2)) * 1.0E-10D

  def getSegment(var1: Array[Double]): Int
}

