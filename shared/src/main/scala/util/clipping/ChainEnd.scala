package util.clipping

//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//


final class ChainEnd(nhead: CurveLink, npartner: ChainEnd) {
  var head: CurveLink =nhead
  var tail: CurveLink = nhead
   var partner: ChainEnd = npartner
   var etag: Int = tail.getEdgeTag

  def getChain: CurveLink = this.head

  def setOtherEnd(partner: ChainEnd): Unit = {
    this.partner = partner
  }

  def getPartner: ChainEnd = this.partner

  def linkTo(that: ChainEnd): CurveLink = if (this.etag != 0 && that.etag != 0) if (this.etag == that.etag) throw new InternalError("Linking chains of the same type!")
  else {
    var enter:ChainEnd = null
    var exit:ChainEnd = null
    if (this.etag == 1) {
      enter = this
      exit = that
    }
    else {
      enter = that
      exit = this
    }
    this.etag = 0
    that.etag = 0
    enter.tail.setNext(exit.head)
    enter.tail = exit.tail
    if (this.partner eq that) enter.head
    else {
      val otherenter = exit.partner
      val otherexit = enter.partner
      otherenter.partner = otherexit
      otherexit.partner = otherenter
      if (enter.head.getYTop < otherenter.head.getYTop) {
        enter.tail.setNext(otherenter.head)
        otherenter.head = enter.head
      }
      else {
        otherexit.tail.setNext(enter.head)
        otherexit.tail = enter.tail
      }
      null
    }
  }
  else throw new InternalError("ChainEnd linked more than once!")

  def addLink(newlink: CurveLink): Unit = {
    if (this.etag == 1) {
      this.tail.setNext(newlink)
      this.tail = newlink
    }
    else {
      newlink.setNext(this.head)
      this.head = newlink
    }
  }

  def getX: Double = if (this.etag == 1) this.tail.getXBot
  else this.head.getXBot
}
