/**
  * Author: Peter Started:30.11.2010
  */
package definition.expression

import java.io.DataOutput
//import java.util.Locale

import definition.typ.DataType


/**
  *
  */
case class CurrencyConstant(n: Long) extends Constant {
  lazy val doubleValue: Double = n.toDouble / 100d

  def getType: DataType.Value = {DataType.CurrencyTyp}

  //def createCopy(): Expression = { new CurrencyConstant(n) }

  def getTerm: String = "%.2f".format(doubleValue) + CurrencyConstant.currencySign

  def toInt: Int = doubleValue.round.toInt

  def toLong: Long = doubleValue.round

  def toDouble: Double = doubleValue

  def toBoolean: Boolean = n > 0

  override def toUnitNumber = new UnitNumber(n.toDouble / 100d, UnitFraction(UnitNumber.setFactory + new UnitElem(CurrencyConstant.currencySign, 1), UnitNumber.emptySet))

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.CurrencyTyp.id)
    //System.out.println("DoubleID:" +DataType.DoubleTyp.id)
    file.writeLong(n)
  }

  def getNative: Double = doubleValue

  override def toCurrency: CurrencyConstant = this

  override def isNumberConstant = true

  def encode: String = "$C" + n.toString

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)

  override def toString: String = getTerm
}


//object ImBroke extends CurrencyConstant(0)

object CurrencyConstant {
  lazy val currencySign = "€"
  val allowedChars: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-')

  def decode(text: String): (Expression, Int) = {
    val end = findEnd(text, 2)
    (new CurrencyConstant(text.substring(2, end).toLong), end)
  }

  def findEnd(text: String, startPos: Int): Int = {
    var pos = startPos
    while (pos < text.length && allowedChars.contains(text.charAt(pos)))
      pos += 1
    pos
  }
}