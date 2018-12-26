package definition.expression


import java.io.{DataInput, DataOutput}

import definition.typ.DataType
import util.Log

import scala.collection.immutable.TreeSet
import scala.collection.{SortedSet, mutable}
import scala.util.control.NonFatal

class UnitElem(val name: String, val exponent: Byte) extends Serializable {
  override def equals(obj: Any): Boolean = obj match {
    case ue: UnitElem => name == ue.name
    case _ => false
  }

  def fullEquals(obj: Any): Boolean = obj match {
    case ue: UnitElem => name == ue.name && exponent == ue.exponent
    case _ => false
  }

  override def hashCode: Int = name.hashCode

  def fullHashCode: Int = name.hashCode + 41 * exponent

  def delta(value: Int) = new UnitElem(name, (exponent + value).toByte)

  def mult(value: Double) = new UnitElem(name, (exponent * value).toByte)

  override def toString: String = name + (if (exponent == 1) "" else exponent)

  def write(out: DataOutput): Unit = {
    out.writeUTF(name)
    out.writeByte(exponent)
  }

}


case class UnitFraction(numerator: SortedSet[UnitElem], denominator: SortedSet[UnitElem]) {
  import UnitNumber._

  def mult(other: UnitFraction): UnitFraction = UnitFraction(addList(numerator, other.numerator), addList(denominator, other.denominator)).trim

  def trim: UnitFraction = {
    var changed = false
    lazy val newNum = numerator.toBuffer
    lazy val newDen = denominator.toBuffer
    for (num <- numerator) denominator.find(_ == num) match {
      case Some(den) =>
        changed = true
        if (num.exponent > den.exponent) {newNum -= num += num.delta(-den.exponent); newDen -= den}
        else if (num.exponent < den.exponent) {newNum -= num; newDen -= den += den.delta(-num.exponent)}
        else {newNum -= num; newDen -= den}
      case None =>
    }
    if (changed) UnitFraction(setFactory ++ newNum, setFactory ++ newDen)
    else this
  }

  private def addList(first: SortedSet[UnitElem], second: SortedSet[UnitElem]) = if (first.isEmpty) second else if (second.isEmpty) first else {
    val result = first.toBuffer
    for (sec <- second) result.indexWhere(_ == sec) match {
      case -1 => result += sec
      case ix => result(ix) = result(ix).delta(sec.exponent)
    }
    setFactory ++ result
  }

  def div(other: UnitFraction): UnitFraction = UnitFraction(addList(numerator, other.denominator), addList(denominator, other.numerator)).trim

  def pot(value: Double) = UnitFraction(setFactory ++ numerator.map(_.mult(value)), setFactory ++ denominator.map(_.mult(value)))

  override def toString: String = if (numerator.isEmpty) {if (denominator.isEmpty) "" else "/" + denominator.mkString("·")}
                                  else numerator.mkString("·") + (if (denominator.isEmpty) "" else "/" + denominator.mkString("·"))

  override def equals(other: Any): Boolean = other match {
    case ofract: UnitFraction => listsFullEquals(numerator, ofract.numerator) && listsFullEquals(denominator, ofract.denominator)
    case _ => false
  }

  override def hashCode: Int = listHashCode(denominator) + listHashCode(numerator)

  def write(out: DataOutput): Unit = {
    out.writeByte(numerator.size)
    for (num <- numerator) num.write(out)
    out.writeByte(denominator.size)
    for (den <- denominator) den.write(out)
  }

  //def sameFraction(other:UnitFraction)=UnitNumber.sameList(numerator,other.numerator)&& UnitNumber.sameList(denominator,other.denominator)

}


case class UnitNumber(value: Double, unitFraction: UnitFraction) extends Constant {

  def getType: DataType.Value = DataType.UnitNumberTyp

  //def createCopy(): Expression = new UnitNumber(value,unitFraction)

  def toInt: Int = value.round.toInt

  def toLong: Long = value.round

  def toDouble: Double = value

  def toBoolean: Boolean = value > 0

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.UnitNumberTyp.id)
    file.writeDouble(value)
    unitFraction.write(file)
  }

  override def toUnitNumber: UnitNumber = this

  def getNative: Double = value

  override def isNumberConstant = true

  def encode: String = "$e" + getTerm

  def getTerm: String = value.toString + unitFraction.toString

  override def getReadableTerm(format: String): String = (
    try {
      format.format(value)
    } catch {
    case NonFatal(e)=> Log.e("format:"+format+" value:"+value+" error:"+e);getTerm
    }) + unitFraction.toString

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)

  override def toString: String = getTerm
}


object UnitNumber {
  val ordering: Ordering[UnitElem] = Ordering.by[UnitElem, String](_.name)
  val emptySet: TreeSet[UnitElem] = collection.immutable.TreeSet[UnitElem]()(ordering)
  val emptyFraction = UnitFraction(emptySet, emptySet)
  val stopChars: Array[Char] = Array(')', ',', ';')

  def listsFullEquals(list1: SortedSet[UnitElem], list2: SortedSet[UnitElem]): Boolean = list1.iterator.corresponds(list2.iterator)(_.fullEquals(_))

  def listHashCode(list: SortedSet[UnitElem]): Int = list.map(_.fullHashCode).sum % Short.MaxValue

  def apply(in: DataInput) = new UnitNumber(in.readDouble, UnitFraction(readList(in), readList(in)))

  private def readList(in: DataInput) =
    setFactory() ++ (for (_ <- 0 until in.readByte) yield new UnitElem(in.readUTF, in.readByte))

  //def sameList(first:SortedSet[UnitElem],second:SortedSet[UnitElem]):Boolean= first.sameElements(second)

  def setFactory() = new mutable.TreeSet()(UnitNumber.ordering)

  def decode(text: String): (UnitNumber, Int) = {
    var fractionEnd = DoubleConstant.findEnd(text, 2)
    while (fractionEnd < text.length && !stopChars.contains(text.charAt(fractionEnd)))
      fractionEnd += 1
    StringParser.parse(text.substring(2, fractionEnd)) match {
      case u: UnitNumber => (u, fractionEnd)
      case o => throw new IllegalArgumentException("error parsing unit number " + text + " " + o)
    }
  }


}