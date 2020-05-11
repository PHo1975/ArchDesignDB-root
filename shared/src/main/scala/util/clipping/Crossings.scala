package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


import java.awt.geom.PathIterator

import _root_.util.Log

import scala.collection.mutable.ArrayBuffer


object Crossings {
  val debug  = false

  def findCrossings(curves: ArrayBuffer[_ <: Curve], xlo: Double, ylo: Double, xhi: Double, yhi: Double): Crossings = {
    val cross  = new Crossings.EvenOdd(xlo, ylo, xhi, yhi)
    val enum_ = curves.iterator
    var c :Curve = null
    do {
        if (! enum_.hasNext ) return cross
        c = enum_.next()
      } while ( {
      ! c.accumulateCrossings(cross)
    })
    null
  }

  def findCrossings(pi: PathIterator, xlo: Double, ylo: Double, xhi: Double, yhi: Double): Crossings = {
    var cross:Crossings  = null
    if (pi.getWindingRule == 0) cross = new Crossings.EvenOdd(xlo, ylo, xhi, yhi)
    else cross = new Crossings.NonZero(xlo, ylo, xhi, yhi)
    val coords  = new Array[Double](23)
    var movx  = 0.0D
    var movy  = 0.0D
    var curx  = 0.0D
    var cury  = .0
    cury = 0.0D
    while ( {
      ! pi.isDone
    }) {
      val `type`  = pi.currentSegment(coords)
      var newx  = .0
      var newy  = .0
      `type` match {
        case 0 =>
          if (movy != cury && cross.asInstanceOf[Crossings] .accumulateLine(curx, cury, movx, movy)) return null
          movx = coords(0); curx = coords(0)
          movy = coords(1);cury = coords(1)

        case 1 =>
          newx = coords(0)
          newy = coords(1)
          if ( cross.asInstanceOf[Crossings] .accumulateLine(curx, cury, newx, newy)) return null
          curx = newx
          cury = newy

        case 2 =>
          newx = coords(2)
          newy = coords(3)
          if ( cross.asInstanceOf[Crossings] .accumulateQuad(curx, cury, coords)) return null
          curx = newx
          cury = newy

        case 3 =>
          newx = coords(4)
          newy = coords(5)
          if ( cross.asInstanceOf[Crossings] .accumulateCubic(curx, cury, coords)) return null
          curx = newx
          cury = newy

        case 4 =>
          if (movy != cury && cross.asInstanceOf[Crossings] .accumulateLine(curx, cury, movx, movy)) return null
          curx = movx
          cury = movy
      }

      pi.next()
    }
    if (movy != cury && cross.asInstanceOf[Crossings] .accumulateLine(curx, cury, movx, movy)) null
    else cross.asInstanceOf[Crossings]
  }

  final class NonZero(nxlo: Double, nylo: Double, nxhi: Double, nyhi: Double) extends Crossings(nxlo, nylo, nxhi, nyhi) {
     private var crosscounts:Array[Int]  = null
    this.crosscounts = new Array[Int](this.yranges.length / 2)

    override def covers(nystart: Double, yend: Double): Boolean = {
      var ystart=nystart
      var i  = 0
      while ( {
        i < this.limit
      }) {
        val ylo  = this.yranges({
          i += 1; i - 1
        })
        val yhi  = this.yranges({
          i += 1; i - 1
        })
        if (ystart < yhi) {
          if (ystart < ylo) return false
          if (yend <= yhi) return true
          ystart = yhi
        }
      }
      ystart >= yend
    }

    def remove(cur: Int): Unit = {
      this.limit -= 2
      val rem  = this.limit - cur
      if (rem > 0) {
        System.arraycopy(this.yranges, cur + 2, this.yranges, cur, rem)
        System.arraycopy(this.crosscounts, cur / 2 + 1, this.crosscounts, cur / 2, rem / 2)
      }
    }

    def insert(cur: Int, lo: Double, hi: Double, dir: Int): Unit = {
      val rem  = this.limit - cur
      val oldranges  = this.yranges
      val oldcounts  = this.crosscounts
      if (this.limit >= this.yranges.length) {
        this.yranges = new Array[Double](this.limit + 10)
        System.arraycopy(oldranges, 0, this.yranges, 0, cur)
        this.crosscounts = new Array[Int]((this.limit + 10) / 2)
        System.arraycopy(oldcounts, 0, this.crosscounts, 0, cur / 2)
      }
      if (rem > 0) {
        System.arraycopy(oldranges, cur, this.yranges, cur + 2, rem)
        System.arraycopy(oldcounts, cur / 2, this.crosscounts, cur / 2 + 1, rem / 2)
      }
      this.yranges(cur + 0) = lo
      this.yranges(cur + 1) = hi
      this.crosscounts(cur / 2) = dir
      this.limit += 2
    }

    override def record(nystart: Double, yend: Double, direction: Int): Unit = {
      var ystart=nystart
      if (ystart < yend) {
        var cur  = 0
        cur = 0
        while ( {
          cur < this.limit && ystart > this.yranges(cur + 1)
        }) cur += 2
        if (cur < this.limit) {
          var rdir  = this.crosscounts(cur / 2)
          var yrlo  = this.yranges(cur + 0)
          var yrhi  = this.yranges(cur + 1)
          if (yrhi == ystart && rdir == direction) {
            if (cur + 2 == this.limit) {
              this.yranges(cur + 1) = yend
              return
            }
            this.remove(cur)
            ystart = yrlo
            rdir = this.crosscounts(cur / 2)
            yrlo = this.yranges(cur + 0)
            yrhi = this.yranges(cur + 1)
          }
          if (yend < yrlo) {
            this.insert(cur, ystart, yend, direction)
            return
          }
          if (yend == yrlo && rdir == direction) {
            this.yranges(cur) = ystart
            return
          }
          if (ystart < yrlo) {
            this.insert(cur, ystart, yrlo, direction)
            cur += 2
            ystart = yrlo
          }
          else if (yrlo < ystart) {
              this.insert(cur, yrlo, ystart, rdir)
              cur += 2
            }
          val newdir  = rdir + direction
          val newend  = Math.min(yend, yrhi)
          if (newdir == 0) this.remove(cur)
          else {
            this.crosscounts(cur / 2) = newdir
            this.yranges({
              cur += 1; cur - 1
            }) = ystart
            this.yranges({
              cur += 1; cur - 1
            }) = newend
          }
          ystart = newend
          if (newend < yrhi) this.insert(cur, newend, yrhi, rdir)
        }
        if (ystart < yend) this.insert(cur, ystart, yend, direction)
      }
    }
  }

  final class EvenOdd(nxlo: Double,nylo: Double,nxhi: Double, nyhi: Double) extends Crossings(nxlo, nylo, nxhi, nyhi) {
    override def covers(ystart: Double, yend: Double): Boolean = this.limit == 2 && this.yranges(0) <= ystart && this.yranges(1) >= yend

    override def record(nystart: Double, nyend: Double, direction: Int): Unit = {
      var ystart=nystart
      var yend=nyend
      if (ystart < yend) {
        var from  = 0
        from = 0
        while ( {
          from < this.limit && ystart > this.yranges(from + 1)
        }) from += 2
        var to  = from
        while ( from < this.limit ) {
          val yrlo  = this.yranges({ from += 1; from - 1 })
          val yrhi  = this.yranges({ from += 1; from - 1 })
          if (yend < yrlo) {
            this.yranges({ to += 1; to - 1 }) = ystart
            this.yranges({ to += 1; to - 1 }) = yend
            ystart = yrlo
            yend = yrhi
          }
          else {
            var yll  = .0
            var ylh  = .0
            if (ystart < yrlo) {
              yll = ystart
              ylh = yrlo
            }
            else {
              yll = yrlo
              ylh = ystart
            }
            var yhl  = .0
            var yhh  = .0
            if (yend < yrhi) {
              yhl = yend
              yhh = yrhi
            }
            else {
              yhl = yrhi
              yhh = yend
            }
            if (ylh == yhl) {
              ystart = yll
              yend = yhh
            }
            else {
              if (ylh > yhl) {
                ystart = yhl
                yhl = ylh
                ylh = ystart
              }
              if (yll != ylh) {
                this.yranges({
                  to += 1; to - 1
                }) = yll
                this.yranges({
                  to += 1; to - 1
                }) = ylh
              }
              ystart = yhl
              yend = yhh
            }
            if (ystart >= yend) Log.w("crossings start >= end")
          }
        }
        if (to < from && from < this.limit) System.arraycopy(this.yranges, from, this.yranges, to, this.limit - from)
        to += this.limit - from
        if (ystart < yend) {
          if (to >= this.yranges.length) {
            val newranges  = new Array[Double](to + 10)
            System.arraycopy(this.yranges, 0, newranges, 0, to)
            this.yranges = newranges
          }
          this.yranges({ to += 1; to - 1 }) = ystart
          this.yranges({ to += 1; to - 1 }) = yend
        }
        this.limit = to
      }
    }
  }
}

abstract class Crossings(var xlo: Double, var ylo: Double, var xhi: Double, var yhi: Double) {
  protected[clipping] var limit  = 0
  protected[clipping] var yranges  = new Array[Double](10)
  private val tmp  = new ArrayBuffer[Curve]()

  final def getXLo: Double = this.xlo

  final def getYLo: Double = this.ylo

  final def getXHi: Double = this.xhi

  final def getYHi: Double = this.yhi

  def record(var1: Double, var3: Double, var5: Int): Unit

  def print(): Unit = {
    System.out.println("Crossings [")
    System.out.println("  bounds = [" + this.ylo + ", " + this.yhi + "]")
    var i  = 0
    while ( {
      i < this.limit
    }) {
      val var10001  = this.yranges(i)
      System.out.println("  [" + var10001 + ", " + this.yranges(i + 1) + "]")

      i += 2
    }
    System.out.println("]")
  }

  final def isEmpty: Boolean = this.limit == 0

  def covers(var1: Double, var3: Double): Boolean

  def accumulateLine(x0: Double, y0: Double, x1: Double, y1: Double): Boolean = if (y0 <= y1) this.accumulateLine(x0, y0, x1, y1, 1)
    else this.accumulateLine(x1, y1, x0, y0, - 1 )

  def accumulateLine(x0: Double, y0: Double, x1: Double, y1: Double, direction: Int): Boolean = if (this.yhi > y0 && this.ylo < y1) if (x0 >= this.xhi && x1 >= this.xhi) false
      else if (y0 != y1) {
          val dx  = x1 - x0
          val dy  = y1 - y0
          var xstart  = .0
          var ystart  = .0
          if (y0 < this.ylo) {
            xstart = x0 + (this.ylo - y0) * dx / dy
            ystart = this.ylo
          }
          else {
            xstart = x0
            ystart = y0
          }
          var yend  = .0
          var xend  = .0
          if (this.yhi < y1) {
            xend = x0 + (this.yhi - y0) * dx / dy
            yend = this.yhi
          }
          else {
            xend = x1
            yend = y1
          }
          if (xstart >= this.xhi && xend >= this.xhi) false
          else if (xstart <= this.xlo && xend <= this.xlo) {
              this.record(ystart, yend, direction)
              false
            }
            else true
        }
        else x0 >= this.xlo || x1 >= this.xlo
    else false

  def accumulateQuad(x0: Double, y0: Double, coords: Array[Double]): Boolean = if (y0 < this.ylo && coords(1) < this.ylo && coords(3) < this.ylo) false
    else if (y0 > this.yhi && coords(1) > this.yhi && coords(3) > this.yhi) false
      else if (x0 > this.xhi && coords(0) > this.xhi && coords(2) > this.xhi) false
        else if (x0 < this.xlo && coords(0) < this.xlo && coords(2) < this.xlo) {
            if (y0 < coords(3)) this.record(Math.max(y0, this.ylo), Math.min(coords(3), this.yhi), 1)
            else if (y0 > coords(3)) this.record(Math.max(coords(3), this.ylo), Math.min(y0, this.yhi), - 1 )
            false
          }
          else {

            Curve.insertQuad(this.tmp, x0, y0, coords)
            val enum_ = this.tmp.iterator
            var c:Curve  = null
            do {
                if (! enum_.hasNext ) {
                  this.tmp.clear()
                  return false
                }
                c = enum_.next().asInstanceOf[Curve]
              } while ( {
              ! c.accumulateCrossings(this)
            })
            true
          }

  def accumulateCubic(x0: Double, y0: Double, coords: Array[Double]): Boolean = if (y0 < this.ylo && coords(1) < this.ylo && coords(3) < this.ylo && coords(5) < this.ylo) false
    else if (y0 > this.yhi && coords(1) > this.yhi && coords(3) > this.yhi && coords(5) > this.yhi) false
      else if (x0 > this.xhi && coords(0) > this.xhi && coords(2) > this.xhi && coords(4) > this.xhi) false
        else if (x0 < this.xlo && coords(0) < this.xlo && coords(2) < this.xlo && coords(4) < this.xlo) {
            if (y0 <= coords(5)) this.record(Math.max(y0, this.ylo), Math.min(coords(5), this.yhi), 1)
            else this.record(Math.max(coords(5), this.ylo), Math.min(y0, this.yhi), - 1 )
            false
          }
          else {
            Curve.insertCubic(this.tmp, x0, y0, coords)
            val enum_ = this.tmp.iterator
            var c:Curve  = null
            do {
                if (! enum_.hasNext ) {
                  this.tmp.clear()
                  return false
                }
                c = enum_.next()
              } while ( {
              ! c.accumulateCrossings(this)
            })
            true
          }
}

