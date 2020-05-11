package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


final class CurveLink(var curve: Curve, var ytop: Double, var ybot: Double, var etag: Int) {
  if (this.ytop < curve.getYTop || this.ybot > curve.getYBot) throw new InternalError("bad curvelink [" + this.ytop + "=>" + this.ybot + "] for " + curve)
   var next:CurveLink = null

  def absorb(link: CurveLink): Boolean = this.absorb(link.curve, link.ytop, link.ybot, link.etag)

  def absorb(curve: Curve, ystart: Double, yend: Double, etag: Int): Boolean = if ((this.curve eq curve) && this.etag == etag && this.ybot >= ystart && this.ytop <= yend) if (ystart >= curve.getYTop && yend <= curve.getYBot) {
    this.ytop = Math.min(this.ytop, ystart)
    this.ybot = Math.max(this.ybot, yend)
    true
  }
  else throw new InternalError("bad curvelink [" + ystart + "=>" + yend + "] for " + curve)
  else false

  def isEmpty: Boolean = this.ytop == this.ybot

  def getCurve: Curve = this.curve

  def getSubCurve: Curve = if (this.ytop == this.curve.getYTop && this.ybot == this.curve.getYBot) this.curve.getWithDirection(this.etag)
  else this.curve.getSubCurve(this.ytop, this.ybot, this.etag)

  def getMoveto = new Order0(this.getXTop, this.getYTop)

  def getXTop: Double = this.curve.XforY(this.ytop)

  def getYTop: Double = this.ytop

  def getXBot: Double = this.curve.XforY(this.ybot)

  def getYBot: Double = this.ybot

  def getX: Double = this.curve.XforY(this.ytop)

  def getEdgeTag: Int = this.etag

  def setNext(link: CurveLink): Unit = {
    this.next = link
  }

  def getNext: CurveLink = this.next
}

