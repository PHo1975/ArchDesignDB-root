package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//




object QuadCurve2D {
  private val BELOW  = - 2
  private val LOWEDGE  = - 1
  private val INSIDE  = 0
  private val HIGHEDGE  = 1
  private val ABOVE  = 2

  /*def getFlatnessSq(x1: scala.Double, y1: scala.Double, ctrlx: scala.Double, ctrly: scala.Double, x2: scala.Double, y2: scala.Double): scala.Double = Line2D.ptSegDistSq(x1, y1, x2, y2, ctrlx, ctrly)

  def getFlatness(x1: scala.Double, y1: scala.Double, ctrlx: scala.Double, ctrly: scala.Double, x2: scala.Double, y2: scala.Double): scala.Double = Line2D.ptSegDist(x1, y1, x2, y2, ctrlx, ctrly)

  def getFlatnessSq(coords: Array[Double], offset: Int): scala.Double = Line2D.ptSegDistSq(coords(offset + 0), coords(offset + 1), coords(offset + 4), coords(offset + 5), coords(offset + 2), coords(offset + 3))

  def getFlatness(coords: Array[Double], offset: Int): scala.Double = Line2D.ptSegDist(coords(offset + 0), coords(offset + 1), coords(offset + 4), coords(offset + 5), coords(offset + 2), coords(offset + 3))*/

  def subdivide(src: QuadCurve2D, left: QuadCurve2D, right: QuadCurve2D): Unit = {
    val x1  = src.getX1
    val y1  = src.getY1
    var ctrlx  = src.getCtrlX
    var ctrly  = src.getCtrlY
    val x2  = src.getX2
    val y2  = src.getY2
    val ctrlx1  = (x1 + ctrlx) / 2.0D
    val ctrly1  = (y1 + ctrly) / 2.0D
    val ctrlx2  = (x2 + ctrlx) / 2.0D
    val ctrly2  = (y2 + ctrly) / 2.0D
    ctrlx = (ctrlx1 + ctrlx2) / 2.0D
    ctrly = (ctrly1 + ctrly2) / 2.0D
    if (left != null) left.setCurve(x1, y1, ctrlx1, ctrly1, ctrlx, ctrly)
    if (right != null) right.setCurve(ctrlx, ctrly, ctrlx2, ctrly2, x2, y2)
  }

  def subdivide(src: Array[scala.Double], srcoff: Int, left: Array[scala.Double], leftoff: Int, right: Array[scala.Double], rightoff: Int): Unit = {
    var x1  = src(srcoff + 0)
    var y1  = src(srcoff + 1)
    var ctrlx  = src(srcoff + 2)
    var ctrly  = src(srcoff + 3)
    var x2  = src(srcoff + 4)
    var y2  = src(srcoff + 5)
    if (left != null) {
      left(leftoff + 0) = x1
      left(leftoff + 1) = y1
    }
    if (right != null) {
      right(rightoff + 4) = x2
      right(rightoff + 5) = y2
    }
    x1 = (x1 + ctrlx) / 2.0D
    y1 = (y1 + ctrly) / 2.0D
    x2 = (x2 + ctrlx) / 2.0D
    y2 = (y2 + ctrly) / 2.0D
    ctrlx = (x1 + x2) / 2.0D
    ctrly = (y1 + y2) / 2.0D
    if (left != null) {
      left(leftoff + 2) = x1
      left(leftoff + 3) = y1
      left(leftoff + 4) = ctrlx
      left(leftoff + 5) = ctrly
    }
    if (right != null) {
      right(rightoff + 0) = ctrlx
      right(rightoff + 1) = ctrly
      right(rightoff + 2) = x2
      right(rightoff + 3) = y2
    }
  }

  def solveQuadratic(eqn: Array[scala.Double]): Int = solveQuadratic(eqn, eqn)

  def solveQuadratic(eqn: Array[scala.Double], res: Array[scala.Double]): Int = {
    val a  = eqn(2)
    val b  = eqn(1)
    val c  = eqn(0)
    var roots  = 0
    if (a == 0.0D) {
      if (b == 0.0D) return - 1
      roots = roots + 1
      res(roots) = - c / b
    }
    else {
      var d  = b * b - 4.0D * a * c
      if (d < 0.0D) return 0
      d = Math.sqrt(d)
      if (b < 0.0D) d = - d
      val q  = (b + d) / - 2.0D
      roots = roots + 1
      res(roots) = q / a
      if (q != 0.0D) res({
          roots += 1; roots - 1
        }) = c / q
    }
    roots
  }

  private def fillEqn(eqn: Array[scala.Double], `val`: scala.Double, c1: scala.Double, cp: scala.Double, c2: scala.Double): Unit = {
    eqn(0) = c1 - `val`
    eqn(1) = cp + cp - c1 - c1
    eqn(2) = c1 - cp - cp + c2
  }

  private def evalQuadratic(vals: Array[scala.Double], num: Int, include0: Boolean, include1: Boolean, inflect: Array[scala.Double],
                            c1: scala.Double, ctrl: scala.Double, c2: scala.Double)  = {
    var j  = 0
    var i=0
    var goOn=true
    while (i < num && goOn) {
      val t  = vals(i)
      if (include0) if (t < 0.0D) goOn=false
      else if (t <= 0.0D) goOn=false
      if (include1) if (t > 1.0D) goOn=false
      else if (t >= 1.0D) goOn=false
      if (goOn&&(inflect == null || inflect(1) + 2.0D * inflect(2) * t != 0.0D)) {
        val u  = 1.0D - t
        vals({
          j += 1; j - 1
        }) = c1 * u * u + 2.0D * ctrl * t * u + c2 * t * t
      }
      i+=1
    }
    j
  }

  private def getTag(coord: scala.Double, low: scala.Double, high: scala.Double)  = if (coord <= low) if (coord < low) - 2
      else - 1
    else if (coord >= high) if (coord > high) 2
        else 1
      else 0

  private def inwards(pttag: Int, opt1tag: Int, opt2tag: Int):Boolean  = pttag match {
      case (-2) =>false
      case 2 =>false

      case (-1) =>
        opt1tag >= 0 || opt2tag >= 0
      case 0 =>
        true
      case 1 =>
        opt1tag <= 0 || opt2tag <= 0
      case _ => false
    }

  @SerialVersionUID(4217149928428559721L)
  class Double() extends QuadCurve2D with Serializable {
    var x1  = .0
    var y1  = .0
    var ctrlx  = .0
    var ctrly  = .0
    var x2  = .0
    var y2  = .0

    def this(x1: scala.Double, y1: scala.Double, ctrlx: scala.Double, ctrly: scala.Double, x2: scala.Double, y2: scala.Double)= {
      this()
      this.setCurve(x1, y1, ctrlx, ctrly, x2, y2)
    }

    override def getX1: scala.Double = this.x1

    override def getY1: scala.Double = this.y1

   // override def getP1  = new Point2D.Double(this.x1, this.y1)

    override def getCtrlX: scala.Double = this.ctrlx

    override def getCtrlY: scala.Double = this.ctrly

    //override def getCtrlPt  = new Point2D.Double(this.ctrlx, this.ctrly)

    override def getX2: scala.Double = this.x2

    override def getY2: scala.Double = this.y2

    //override def getP2  = new Point2D.Double(this.x2, this.y2)

    override def setCurve(x1: scala.Double, y1: scala.Double, ctrlx: scala.Double, ctrly: scala.Double, x2: scala.Double, y2: scala.Double): Unit = {
      this.x1 = x1
      this.y1 = y1
      this.ctrlx = ctrlx
      this.ctrly = ctrly
      this.x2 = x2
      this.y2 = y2
    }

    /*override def getBounds2D: Rectangle2D = {
      val left  = Math.min(Math.min(this.x1, this.x2), this.ctrlx)
      val top  = Math.min(Math.min(this.y1, this.y2), this.ctrly)
      val right  = Math.max(Math.max(this.x1, this.x2), this.ctrlx)
      val bottom  = Math.max(Math.max(this.y1, this.y2), this.ctrly)
      new Rectangle2D.Double(left, top, right - left, bottom - top)
    }*/
  }

  

}

abstract class QuadCurve2D protected() {
  def getX1: scala.Double

  def getY1: scala.Double  

  def getCtrlX: scala.Double

  def getCtrlY: scala.Double

    def getX2: scala.Double

  def getY2: scala.Double

    def setCurve(var1: scala.Double, var3: scala.Double, var5: scala.Double, var7: scala.Double, var9: scala.Double, var11: scala.Double): Unit

  def setCurve(coords: Array[Double], offset: Int): Unit = {
    this.setCurve(coords(offset + 0), coords(offset + 1), coords(offset + 2), coords(offset + 3), coords(offset + 4), coords(offset + 5))
  }
 
  def setCurve(c: QuadCurve2D): Unit = {
    this.setCurve(c.getX1, c.getY1, c.getCtrlX, c.getCtrlY, c.getX2, c.getY2)
  }

/*  def getFlatnessSq: scala.Double = Line2D.ptSegDistSq(this.getX1, this.getY1, this.getX2, this.getY2, this.getCtrlX, this.getCtrlY)

  def getFlatness: scala.Double = Line2D.ptSegDist(this.getX1, this.getY1, this.getX2, this.getY2, this.getCtrlX, this.getCtrlY)
*/
  def subdivide(left: QuadCurve2D, right: QuadCurve2D): Unit = {
    QuadCurve2D.subdivide(this, left, right)
  }

  def contains(x: scala.Double, y: scala.Double): Boolean = {
    val x1  = this.getX1
    val y1  = this.getY1
    val xc  = this.getCtrlX
    val yc  = this.getCtrlY
    val x2  = this.getX2
    val y2  = this.getY2
    val kx  = x1 - 2.0D * xc + x2
    val ky  = y1 - 2.0D * yc + y2
    val dx  = x - x1
    val dy  = y - y1
    val dxl  = x2 - x1
    val dyl  = y2 - y1
    val t0  = (dx * ky - dy * kx) / (dxl * ky - dyl * kx)
    if (t0 >= 0.0D && t0 <= 1.0D && t0 == t0) {
      val xb  = kx * t0 * t0 + 2.0D * (xc - x1) * t0 + x1
      val yb  = ky * t0 * t0 + 2.0D * (yc - y1) * t0 + y1
      val xl  = dxl * t0 + x1
      val yl  = dyl * t0 + y1
      x >= xb && x < xl || x >= xl && x < xb || y >= yb && y < yl || y >= yl && y < yb
    }
    else false
  }

  //override def contains(p: Point2D): Boolean = this.contains(p.getX, p.getY)

  def intersects(x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean = if (w > 0.0D && h > 0.0D) {
      val x1  = this.getX1
      val y1  = this.getY1
      val x1tag  = QuadCurve2D.getTag(x1, x, x + w)
      val y1tag  = QuadCurve2D.getTag(y1, y, y + h)
      if (x1tag == 0 && y1tag == 0) true
      else {
        val x2  = this.getX2
        val y2  = this.getY2
        val x2tag  = QuadCurve2D.getTag(x2, x, x + w)
        val y2tag  = QuadCurve2D.getTag(y2, y, y + h)
        if (x2tag == 0 && y2tag == 0) true
        else {
          val ctrlx  = this.getCtrlX
          val ctrly  = this.getCtrlY
          val ctrlxtag  = QuadCurve2D.getTag(ctrlx, x, x + w)
          val ctrlytag  = QuadCurve2D.getTag(ctrly, y, y + h)
          if (x1tag < 0 && x2tag < 0 && ctrlxtag < 0) false
          else if (y1tag < 0 && y2tag < 0 && ctrlytag < 0) false
            else if (x1tag > 0 && x2tag > 0 && ctrlxtag > 0) false
              else if (y1tag > 0 && y2tag > 0 && ctrlytag > 0) false
                else if (QuadCurve2D.inwards(x1tag, x2tag, ctrlxtag) && QuadCurve2D.inwards(y1tag, y2tag, ctrlytag)) true
                  else if (QuadCurve2D.inwards(x2tag, x1tag, ctrlxtag) && QuadCurve2D.inwards(y2tag, y1tag, ctrlytag)) true
                    else {
                      val xoverlap  = x1tag * x2tag <= 0
                      val yoverlap  = y1tag * y2tag <= 0
                      if (x1tag == 0 && x2tag == 0 && yoverlap) true
                      else if (y1tag == 0 && y2tag == 0 && xoverlap) true
                        else {
                          val eqn  = new Array[Double](3)
                          val res  = new Array[Double](3)
                          if (! yoverlap ) {
                            QuadCurve2D.fillEqn(eqn, if (y1tag < 0) y
                            else y + h, y1, ctrly, y2)
                            QuadCurve2D.solveQuadratic(eqn, res) == 2 && QuadCurve2D.evalQuadratic(res, 2, true, true, null.asInstanceOf[Array[Double]], x1, ctrlx, x2) == 2 && QuadCurve2D.getTag(res(0), x, x + w) * QuadCurve2D.getTag(res(1), x, x + w) <= 0
                          }
                          else if (! xoverlap ) {
                              QuadCurve2D.fillEqn(eqn, if (x1tag < 0) x
                              else x + w, x1, ctrlx, x2)
                              QuadCurve2D.solveQuadratic(eqn, res) == 2 && QuadCurve2D.evalQuadratic(res, 2, true, true, null.asInstanceOf[Array[Double]], y1, ctrly, y2) == 2 && QuadCurve2D.getTag(res(0), y, y + h) * QuadCurve2D.getTag(res(1), y, y + h) <= 0
                            }
                            else {
                              val dx  = x2 - x1
                              val dy  = y2 - y1
                              val k  = y2 * x1 - x2 * y1
                              var c1tag  = 0
                              if (y1tag == 0) c1tag = x1tag
                              else c1tag = QuadCurve2D.getTag((k + dx * (if (y1tag < 0) y
                                else y + h)) / dy, x, x + w)
                              var c2tag  = 0
                              if (y2tag == 0) c2tag = x2tag
                              else c2tag = QuadCurve2D.getTag((k + dx * (if (y2tag < 0) y
                                else y + h)) / dy, x, x + w)
                              if (c1tag * c2tag <= 0) true
                              else {
                                c1tag = if (c1tag * x1tag <= 0) y1tag
                                else y2tag
                                QuadCurve2D.fillEqn(eqn, if (c2tag < 0) x
                                else x + w, x1, ctrlx, x2)
                                val num  = QuadCurve2D.solveQuadratic(eqn, res)
                                QuadCurve2D.evalQuadratic(res, num, true, true, null.asInstanceOf[Array[Double]], y1, ctrly, y2)
                                c2tag = QuadCurve2D.getTag(res(0), y, y + h)
                                c1tag * c2tag <= 0
                              }
                            }
                        }
                    }
        }
      }
    }
    else false

  //override def intersects(r: Rectangle2D): Boolean = this.intersects(r.getX, r.getY, r.getWidth, r.getHeight)

   def contains(x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean = if (w > 0.0D && h > 0.0D) this.contains(x, y) && this.contains(x + w, y) && this.contains(x + w, y + h) && this.contains(x, y + h)
    else false

  //override def contains(r: Rectangle2D): Boolean = this.contains(r.getX, r.getY, r.getWidth, r.getHeight)

  //override def getBounds: Rectangle = this.getBounds2D.getBounds

  //override def getPathIterator(at: AffineTransform)  = new QuadIterator(this, at)

  //override def getPathIterator(at: AffineTransform, flatness: scala.Double)  = new FlatteningPathIterator(this.getPathIterator(at), flatness)

  /*override def clone: Any = try super.clone
    catch {
      case var2: CloneNotSupportedException =>
        throw new InternalError(var2)
    }*/
}

