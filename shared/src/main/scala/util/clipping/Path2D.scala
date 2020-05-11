package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


object Path2D {
  val WIND_EVEN_ODD  = 0
  val WIND_NON_ZERO  = 1
  private val SEG_MOVETO  = 0
  private val SEG_LINETO  = 1
  private val SEG_QUADTO  = 2
  private val SEG_CUBICTO  = 3
  private val SEG_CLOSE  = 4
  private[clipping] val INIT_SIZE  = 20
  private[clipping] val EXPAND_MAX  = 500
  private[clipping] val EXPAND_MAX_COORDS  = 1000
  private[clipping] val EXPAND_MIN  = 10
  private val SERIAL_STORAGE_FLT_ARRAY  = 48
  private val SERIAL_STORAGE_DBL_ARRAY  = 49
  private val SERIAL_SEG_FLT_MOVETO  = 64
  private val SERIAL_SEG_FLT_LINETO  = 65
  private val SERIAL_SEG_FLT_QUADTO  = 66
  private val SERIAL_SEG_FLT_CUBICTO  = 67
  private val SERIAL_SEG_DBL_MOVETO  = 80
  private val SERIAL_SEG_DBL_LINETO  = 81
  private val SERIAL_SEG_DBL_QUADTO  = 82
  private val SERIAL_SEG_DBL_CUBICTO  = 83
  private val SERIAL_SEG_CLOSE  = 96
  private val SERIAL_PATH_END  = 97

  private[clipping] def expandPointTypes(oldPointTypes: Array[Byte], needed: Int): Array[Byte] = {
    val oldSize  = oldPointTypes.length
    val newSizeMin  = oldSize + needed
    if (newSizeMin < oldSize) throw new ArrayIndexOutOfBoundsException("pointTypes exceeds maximum capacity !")
    else {
      var grow  = oldSize
      if (oldSize > 500) grow = Math.max(500, oldSize >> 3)
      else if (oldSize < 10) grow = 10
      assert(grow > 0)
      var newSize  = oldSize + grow
      if (newSize < newSizeMin) newSize = 2147483647
      while ( true ) try return java.util.Arrays.copyOf(oldPointTypes, newSize)
        catch {
          case var7: OutOfMemoryError =>
            if (newSize == newSizeMin) throw var7
            newSize = newSizeMin + (newSize - newSizeMin) / 2
        }
      Array.empty
    }
  }

  def contains(pi: PathIterator, x: scala.Double, y: scala.Double): Boolean = if (x * 0.0D + y * 0.0D == 0.0D) {
      val mask  = if (pi.getWindingRule == 1) - 1
      else 1
      val cross  = Curve.pointCrossingsForPath(pi, x, y)
      (cross & mask) != 0
    }
    else false

  //def contains(pi: PathIterator, p: Point2D): Boolean = contains(pi, p.getX, p.getY)

  def contains(pi: PathIterator, x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean = if (! java.lang.Double.isNaN(x + w) && ! java.lang.Double.isNaN(y + h) ) if (w > 0.0D && h > 0.0D) {
        val mask  = if (pi.getWindingRule == 1) - 1
        else 2
        val crossings  = Curve.rectCrossingsForPath(pi, x, y, x + w, y + h)
        crossings != - 2147483648 && (crossings & mask) != 0
      }
      else false
    else false

  //def contains(pi: PathIterator, r: Rectangle2D): Boolean = contains(pi, r.getX, r.getY, r.getWidth, r.getHeight)

  def intersects(pi: PathIterator, x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean = if (! java.lang.Double.isNaN(x + w) && ! java.lang.Double.isNaN(y + h) ) if (w > 0.0D && h > 0.0D) {
        val mask  = if (pi.getWindingRule == 1) - 1
        else 2
        val crossings  = Curve.rectCrossingsForPath(pi, x, y, x + w, y + h)
        crossings == - 2147483648 || (crossings & mask) != 0
      }
      else false
    else false

  //def intersects(pi: PathIterator, r: Rectangle2D): Boolean = intersects(pi, r.getX, r.getY, r.getWidth, r.getHeight)

  private[clipping] object Iterator {
  protected [clipping] val curvecoords  = Array[Int](2, 2, 4, 6, 0)
  }

  abstract private[clipping] class Iterator private[clipping](var path: Path2D) extends PathIterator {
    private[clipping] var typeIdx  = 0
    private[clipping] var pointIdx  = 0

    override def getWindingRule: Int = this.path.getWindingRule

    override def isDone: Boolean = this.typeIdx >= this.path.numTypes

    override def next(): Unit = {
      val `type`  = this.path.pointTypes({
        this.typeIdx += 1; this.typeIdx - 1
      })
      this.pointIdx += Iterator.curvecoords(`type`)
    }
  }

  @SerialVersionUID(1826762518450014216L)
  object Double {
    private[clipping] def expandCoords(oldCoords: Array[scala.Double], needed: Int): Array[scala.Double] = {
      val oldSize  = oldCoords.length
      val newSizeMin  = oldSize + needed
      if (newSizeMin < oldSize) throw new ArrayIndexOutOfBoundsException("coords exceeds maximum capacity !")
      else {
        var grow  = oldSize
        if (oldSize > 1000) grow = Math.max(1000, oldSize >> 3)
        else if (oldSize < 10) grow = 10
        assert(grow > needed)
        var newSize  = oldSize + grow
        if (newSize < newSizeMin) newSize = 2147483647
        while ( {
          true
        }) try return java.util.Arrays.copyOf(oldCoords, newSize)
          catch {
            case var7: OutOfMemoryError =>
              if (newSize == newSizeMin) throw var7
              newSize = newSizeMin + (newSize - newSizeMin) / 2
          }
      }
      Array.empty
    }

    /*private[clipping] class TxIterator private[clipping](val p2dd: Path2D.Double, var affine: AffineTransform) extends Path2D.Iterator(p2dd) {
      this.doubleCoords = p2dd.doubleCoords
      private[clipping] var doubleCoords  = null

      override def currentSegment(coords: Array[Float]): Int = {
        val `type`  = this.path.pointTypes(this.typeIdx)
        val numCoords  = curvecoords(`type`)
        if (numCoords > 0) this.affine.transform(this.doubleCoords, this.pointIdx, coords, 0, numCoords / 2)
        `type`
      }

      override def currentSegment(coords: Array[Double]): Int = {
        val `type`  = this.path.pointTypes(this.typeIdx)
        val numCoords  = curvecoords(`type`)
        if (numCoords > 0) this.affine.transform(this.doubleCoords, this.pointIdx, coords, 0, numCoords / 2)
        `type`
      }
    }*/

    private[clipping] class CopyIterator private[clipping](val p2dd: Path2D.Double) extends Path2D.Iterator(p2dd) {

      private[clipping] var doubleCoords:Array[scala.Double]  = p2dd.doubleCoords

      override def currentSegment(coords: Array[Float]): Int = {
        val typer  = this.path.pointTypes(this.typeIdx)
        val numCoords  = Iterator.curvecoords(typer)
        if (numCoords > 0) for (i <- 0 until numCoords) {
            coords(i) = this.doubleCoords(this.pointIdx + i).toFloat
          }
        typer
      }

      override def currentSegment(coords: Array[scala.Double]): Int = {
        val typer  = this.path.pointTypes(this.typeIdx)
        val numCoords  = Iterator.curvecoords(typer)
        if (numCoords > 0) System.arraycopy(this.doubleCoords, this.pointIdx, coords, 0, numCoords)
        typer
      }
    }

  }

  @SerialVersionUID(1826762518450014216L)
  class Double() extends Path2D(1, 20) with Serializable with PathContainer {

        private[clipping] var doubleCoords  = new Array[scala.Double](20 * 2)



    override final def trimToSize(): Unit = {
      if (this.numTypes < this.pointTypes.length) this.pointTypes = java.util.Arrays.copyOf(this.pointTypes, this.numTypes)
      if (this.numCoords < this.doubleCoords.length) this.doubleCoords = java.util.Arrays.copyOf(this.doubleCoords, this.numCoords)
    }

    /*override private[clipping] def cloneCoordsFloat(at: AffineTransform)  = {
      val ret  = new Array[Float](this.numCoords)
      if (at == null) for (i <- 0 until this.numCoords) {
          ret(i) = this.doubleCoords(i).toFloat
        }
      else at.transform(this.doubleCoords, 0, ret, 0, this.numCoords / 2)
      ret
    }

    override private[clipping] def cloneCoordsDouble(at: AffineTransform)  = {
      var ret  = null
      if (at == null) ret = Arrays.copyOf(this.doubleCoords, this.numCoords)
      else {
        ret = new Array[Double](this.numCoords)
        at.transform(this.doubleCoords, 0, ret, 0, this.numCoords / 2)
      }
      ret
    }*/

    override private[clipping] def append(x: Float, y: Float): Unit = {
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x.toDouble
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y.toDouble
    }

    override private[clipping] def append(x: scala.Double, y: scala.Double): Unit = {
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y
    }

    //override private[clipping] def getPoint(coordindex: Int)  = new Point2D.Double(this.doubleCoords(coordindex), this.doubleCoords(coordindex + 1))

    override private[clipping] def needRoom(needMove: Boolean, newCoords: Int): Unit = {
      if (this.numTypes == 0 && needMove) throw new IllegalPathStateException("missing initial moveto in path definition")
      else {
        if (this.numTypes >= this.pointTypes.length) this.pointTypes = expandPointTypes(this.pointTypes, 1)
        if (this.numCoords > this.doubleCoords.length - newCoords) this.doubleCoords = Double.expandCoords(this.doubleCoords, newCoords)
      }
    }

    override final def moveTo(x: scala.Double, y: scala.Double): Unit = {
      if (this.numTypes > 0 && this.pointTypes(this.numTypes - 1) == 0) {
        this.doubleCoords(this.numCoords - 2) = x
        this.doubleCoords(this.numCoords - 1) = y
      }
      else {
        this.needRoom(false, 2)
        this.pointTypes({
          this.numTypes += 1; this.numTypes - 1
        }) = 0
        this.doubleCoords({
          this.numCoords += 1; this.numCoords - 1
        }) = x
        this.doubleCoords({
          this.numCoords += 1; this.numCoords - 1
        }) = y
      }
    }

    override final def lineTo(x: scala.Double, y: scala.Double): Unit = {
      this.needRoom(true, 2)
      this.pointTypes({
        this.numTypes += 1; this.numTypes - 1
      }) = 1
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y
    }

    override final def quadTo(x1: scala.Double, y1: scala.Double, x2: scala.Double, y2: scala.Double): Unit = {
      this.needRoom(true, 4)
      this.pointTypes({
        this.numTypes += 1; this.numTypes - 1
      }) = 2
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x1
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y1
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x2
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y2
    }

    override final def curveTo(x1: scala.Double, y1: scala.Double, x2: scala.Double, y2: scala.Double, x3: scala.Double, y3: scala.Double): Unit = {
      this.needRoom(true, 6)
      this.pointTypes({
        this.numTypes += 1; this.numTypes - 1
      }) = 3
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x1
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y1
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x2
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y2
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = x3
      this.doubleCoords({
        this.numCoords += 1; this.numCoords - 1
      }) = y3
    }

    override private[clipping] def pointCrossings(px: scala.Double, py: scala.Double)  = if (this.numTypes == 0) 0
      else {
        val coords  = this.doubleCoords

        var curx  =  coords(0)
        var  movx = coords(0)
        var cury  = coords(1)
        var movy=cury
        var crossings:Int  = 0
        var ci  = 2
        for (i <- 1 until this.numTypes) {
          var endx  = .0
          var endy  = .0
          this.pointTypes(i) match {
            case 0 =>
              if (cury != movy) crossings += Curve.pointCrossingsForLine(px, py, curx, cury, movx, movy)
              movx = coords({
                ci += 1; ci - 1
              })
              curx=movx
              movy = coords({
                ci += 1; ci - 1
              })
              cury =movy

            case 1 =>
              endx = coords({ ci += 1; ci - 1})
              endy = coords({ ci += 1; ci - 1})
              crossings += Curve.pointCrossingsForLine(px, py, curx, cury,endx , endy)
              curx = endx
              cury = endy

            case 2 =>
              crossings += Curve.pointCrossingsForQuad(px, py, curx, cury, coords({ ci += 1; ci - 1 }), coords({ ci += 1; ci - 1 }),
                {endx = coords({ ci += 1; ci - 1 });endx}, {endy = coords({  ci += 1; ci - 1 });endy}, 0)
              curx = endx
              cury = endy

            case 3 =>
              crossings += Curve.pointCrossingsForCubic(px, py, curx, cury, coords({ ci += 1; ci - 1 }), coords({
                ci += 1; ci - 1
              }), coords({ ci += 1; ci - 1 }), coords({ ci += 1; ci - 1 }),
                {endx = coords({  ci += 1; ci - 1 });endx},
                {endy = coords({  ci += 1; ci - 1 });endy}, 0)
              curx = endx
              cury = endy

            case 4 =>
              if (cury != movy) crossings += Curve.pointCrossingsForLine(px, py, curx, cury, movx, movy)
              curx = movx
              cury = movy
          }
        }
        if (cury != movy) crossings += Curve.pointCrossingsForLine(px, py, curx, cury, movx, movy)
        crossings
      }

    override private[clipping] def rectCrossings(rxmin: scala.Double, rymin: scala.Double, rxmax: scala.Double, rymax: scala.Double)  = if (this.numTypes == 0) 0
      else {
        val coords  = this.doubleCoords

        var curx  = coords(0)
        var movx=curx
        var cury  = coords(1)
        var movy=cury
        var crossings  = 0
        var ci  = 2
        var i  = 1
        while ( {
          crossings != - 2147483648 && i < this.numTypes
        }) {
          var endx  = .0
          var endy  = .0
          this.pointTypes(i) match {
            case 0 =>
              if (curx != movx || cury != movy) crossings = Curve.rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
              movx = coords({
                ci += 1; ci - 1
              })
              curx =movx
                movy =  coords({
                ci += 1; ci - 1
              })
              cury = movy

            case 1 =>
              endx = coords({
                ci += 1; ci - 1
              })
              endy = coords({
                ci += 1; ci - 1
              })
              crossings = Curve.rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, endx, endy)
              curx = endx
              cury = endy

            case 2 =>
              crossings = Curve.rectCrossingsForQuad(crossings, rxmin, rymin, rxmax, rymax, curx, cury, coords({  ci += 1; ci - 1 }),
                coords({ ci += 1; ci - 1 }), {endx = coords({ci += 1; ci - 1});endx},
                {endy = coords({ ci += 1; ci - 1 });endy}, 0)
              curx = endx
              cury = endy

            case 3 =>
              crossings = Curve.rectCrossingsForCubic(crossings, rxmin, rymin, rxmax, rymax, curx, cury, coords({
                ci += 1; ci - 1
              }), coords({  ci += 1; ci - 1 }), coords({ci += 1; ci - 1 }), coords({ ci += 1; ci - 1 }),
                {endx = coords({ ci += 1; ci - 1 });endx},
                {endy = coords({ ci += 1; ci - 1 });endy}, 0)
              curx = endx
              cury = endy

            case 4 =>
              if (curx != movx || cury != movy) crossings = Curve.rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
              curx = movx
              cury = movy
          }

          i += 1
        }
        if (crossings != - 2147483648 && (curx != movx || cury != movy)) crossings = Curve.rectCrossingsForLine(crossings, rxmin, rymin, rxmax, rymax, curx, cury, movx, movy)
        crossings
      }

    override final def append(pi: PathIterator, nconnect: Boolean): Unit = {
      var connect=nconnect
      val coords  = new Array[scala.Double](6)
      var goOn=true
      while (   ! pi.isDone &&goOn) {
        pi.currentSegment(coords) match {
          case 0 =>
            if (connect && this.numTypes >= 1 && this.numCoords >= 1) {
              if (this.pointTypes(this.numTypes - 1) == 4 || this.doubleCoords(this.numCoords - 2) != coords(0) ||
                this.doubleCoords(this.numCoords - 1) != coords(1)) this.lineTo(coords(0), coords(1))
              goOn=false

            }
            if(goOn) this.moveTo(coords(0), coords(1))

          case 1 =>
            this.lineTo(coords(0), coords(1))

          case 2 =>
            this.quadTo(coords(0), coords(1), coords(2), coords(3))

          case 3 =>
            this.curveTo(coords(0), coords(1), coords(2), coords(3), coords(4), coords(5))

          case 4 =>
            this.closePath()
        }
        pi.next()

        connect = false
      }
    }

    /*override final def transform(at: AffineTransform): Unit = {
      at.transform(this.doubleCoords, 0, this.doubleCoords, 0, this.numCoords / 2)
    }*/

    /*override final def getBounds2D: Rectangle2D = {
      var i  = this.numCoords
      var x1  = .0
      var y1  = .0
      var x2  = .0
      var y2  = .0
      if (i > 0) {
        i -= 1
        y1 = y2 = this.doubleCoords(i)
        i -= 1
        x1 = x2 = this.doubleCoords(i)
        while ( {
          i > 0
        }) {
          i -= 1
          val y  = this.doubleCoords(i)
          i -= 1
          val x  = this.doubleCoords(i)
          if (x < x1) x1 = x
          if (y < y1) y1 = y
          if (x > x2) x2 = x
          if (y > y2) y2 = y
        }
      }
      else {
        y2 = 0.0D
        x2 = 0.0D
        y1 = 0.0D
        x1 = 0.0D
      }
      new Rectangle2D.Double(x1, y1, x2 - x1, y2 - y1)
    }*/

    def getPathIterator(): PathIterator = new Double.CopyIterator(this)

    //override final def clone  = new Path2D.Double(this)
  }



abstract class Path2D(rule: Int, initialTypes: Int) {
  private[clipping] var pointTypes = new Array[Byte](initialTypes)
  private[clipping] var numTypes = 0
  private[clipping] var numCoords = 0
  private[clipping] var windingRule = 0

  this.setWindingRule(rule)


  private[clipping] def append(var1: Float, var2: Float): Unit

  private[clipping] def append(var1: scala.Double, var3: scala.Double): Unit

  //private[clipping] def getPoint(var1: Int)

  private[clipping] def needRoom(var1: Boolean, var2: Int): Unit

  private[clipping] def pointCrossings(var1: scala.Double, var3: scala.Double): Int

  private[clipping] def rectCrossings(var1: scala.Double, var3: scala.Double, var5: scala.Double, var7: scala.Double): Int

  def moveTo(var1: scala.Double, var3: scala.Double): Unit

  def lineTo(var1: scala.Double, var3: scala.Double): Unit

  def quadTo(var1: scala.Double, var3: scala.Double, var5: scala.Double, var7: scala.Double): Unit

  def curveTo(var1: scala.Double, var3: scala.Double, var5: scala.Double, var7: scala.Double, var9: scala.Double, var11: scala.Double): Unit

  final def closePath(): Unit = {
    if (this.numTypes == 0 || this.pointTypes(this.numTypes - 1) != 4) {
      this.needRoom(true, 0)
      this.numTypes += 1;
      this.pointTypes(this.numTypes - 1) = 4
    }
  }


  def append(var1: PathIterator, var2: Boolean): Unit

  final def getWindingRule: Int = this.windingRule

  final def setWindingRule(rule: Int): Unit = {
    if (rule != 0 && rule != 1) throw new IllegalArgumentException("winding rule must be WIND_EVEN_ODD or WIND_NON_ZERO")
    else this.windingRule = rule
  }

  /*final def getCurrentPoint: Point2D = {
    var index  = this.numCoords
    if (this.numTypes >= 1 && index >= 1) {
      if (this.pointTypes(this.numTypes - 1) == 4) for (i <- this.numTypes - 2 until 0 by -1) {
          this.pointTypes(i) match {
            case 0 =>
              return this.getPoint(index - 2)
            case 1 =>
              index -= 2

            case 2 =>
              index -= 4

            case 3 =>
              index -= 6
            case 4 =>
          }
        }
      this.getPoint(index - 2)
    }
    else null
  }*/

  final def reset(): Unit = {
    this.numTypes = 0
    this.numCoords = 0
  }

  //def transform(var1: AffineTransform): Unit

  /*final def createTransformedShape(at: AffineTransform): Shape = {
    val p2d  = this.clone.asInstanceOf[Path2D]
    if (at != null) p2d.transform(at)
    p2d
  }

  override final def getBounds: Rectangle = this.getBounds2D.getBounds*/

  final def contains(x: scala.Double, y: scala.Double): Boolean = if (x * 0.0D + y * 0.0D == 0.0D)
    if (this.numTypes < 2) false
    else {
      val mask = if (this.windingRule == 1) -1 else 1
      (this.pointCrossings(x, y) & mask) != 0
    }
  else false

  //final def contains(p: Point2D): Boolean = this.contains(p.getX, p.getY)

  def contains(x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean = if (!java.lang.Double.isNaN(x + w) && !java.lang.Double.isNaN(y + h)) if (w > 0.0D && h > 0.0D) {
    val mask = if (this.windingRule == 1) -1
    else 2
    val crossings = this.rectCrossings(x, y, x + w, y + h)
    crossings != -2147483648 && (crossings & mask) != 0
  }
  else false
  else false

  //override final def contains(r: Rectangle2D): Boolean = this.contains(r.getX, r.getY, r.getWidth, r.getHeight)

  final def intersects(x: scala.Double, y: scala.Double, w: scala.Double, h: scala.Double): Boolean =
    if (!java.lang.Double.isNaN(x + w) && !java.lang.Double.isNaN(y + h)) if (w > 0.0D && h > 0.0D) {
      val mask = if (this.windingRule == 1) -1
      else 2
      val crossings = this.rectCrossings(x, y, x + w, y + h)
      crossings == -2147483648 || (crossings & mask) != 0
    }
    else false
    else false

  //override final def intersects(r: Rectangle2D): Boolean = this.intersects(r.getX, r.getY, r.getWidth, r.getHeight)

  //def getPathIterator( flatness: scala.Double)  = new FlatteningPathIterator(this.getPathIterator(at), flatness)


  def trimToSize(): Unit

}
}
