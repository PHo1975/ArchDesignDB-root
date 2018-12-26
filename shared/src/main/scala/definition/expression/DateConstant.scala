/**
  * Author: Peter Started:24.04.2011
  */
package definition.expression

import java.io.{DataInput, DataOutput}

import definition.typ.DataType
import util.StrToInt


case class DateConstant(day: Int, month: Int, year: Int) extends Constant {

  lazy val julian: Long = DateConstant.julian(day, month, year)

  def addMonth(): DateConstant = if (month == 12) DateConstant(day, 1, year + 1) else {
    val res = DateConstant(day, month + 1, year)
    if (res.month > month + 1) res.addDays(-res.day) else res
  }

  def subMonth(): DateConstant = if (month == 1) DateConstant(day, 12, year - 1) else {
    val res = DateConstant(day, month - 1, year)
    if (res.month > month - 1) res.addDays(-res.day) else res
  }

  def addDays(ndays: Int): DateConstant = DateConstant.asJulian(julian + ndays)

  def dayDistance(other: DateConstant): Int = (other.julian - julian).toInt

  def weekOfYear: Int = {
    val thursday = julian + 3 - weekDay
    val kyear = DateConstant.yearFromJulian(thursday)
    val first = DateConstant.julian(4, 1, kyear) + 3 - DateConstant.getWeekDayInYear(kyear, 4)
    (thursday - first).toInt / 7 + 1
  }

  def weekDay: Int = DateConstant.getWeekDayInYear(year, dayInYear())

  def dayInYear(): Int = {
    val d = (month + 10) / 13
    val e = day + (611 * (month + 2)) / 20 - 2 * d - 91
    e + DateConstant.leapYear(year) * d
  }

  override def equals(other: Any): Boolean = other match {
    case that: DateConstant => (that canEqual this) && day == that.day && month == that.month && year == that.year
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[DateConstant]

  // *********************** interface Constant ************************************


  def toInt: Int = 0

  def toDouble: Double = julian.toDouble

  def toBoolean: Boolean = true

  def getNative: Any = this

  def getType: DataType.Value = DataType.DateTyp

  def getTerm: String = toString

  //def createCopy(): Expression = { new DateConstant(day,month,year) }

  override def toString: String = day + "." + month + "." + year

  def toDateString: String = toString

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.DateTyp.id)
    file.writeLong((julian - DateConstant.julian1970) * 86400000L)
  }

  override def toDate: DateConstant = this

  def sameDay(other: DateConstant): Boolean = year == other.year && month == other.month &&
    day == other.day

  def encode: String = "$U" + toLong

  def toLong: Long = julian

  override def containsString(st: String, checkNumbers: Boolean): Boolean = checkNumbers && toString.contains(st)
}


object DateConstant {

  lazy val NULL_DATE = DateConstant(0, 0, 0)

  val julian1970: Long = julian(1, 1, 1970)
  val nativeNull = 0L

  def apply(): DateConstant = {
    asJulian(julian1970 + System.currentTimeMillis() / 86400000L)
  }

  def asJulian(jday: Long): DateConstant = {
    val jd1 = jday - 1721119L
    var j = (4L * jd1 - 1L) / 146097L
    var jd = (4L * jd1 - 1L) % 146097L
    var t = jd / 4L
    jd = (4L * t + 3L) / 1461L
    t = (((4L * t + 3L) % 1461L) + 4L) / 4L
    val m1 = (5L * t - 3L) / 153L
    t = (((5L * t - 3L) % 153L) + 5L) / 5L
    j = 100L * j + jd
    val me = if (m1 < 10L) m1 + 3 else m1 - 9
    val je = if (m1 < 10L) j else j + 1
    apply(t.toInt, me.toInt, je.toInt)
  }

  def unapply(st: String): Option[DateConstant] =
    st.split('.') match {
      case Array(StrToInt(d), StrToInt(m), StrToInt(y)) if d > 0 && d < 32 && m > 0 && m < 13 && y > 0 => Some(DateConstant(d, m, y))
      case _ => None
    }

  def yearFromJulian(jday: Long): Int = {
    val jd1 = jday - 1721119L
    val j = (4L * jd1 - 1L) / 146097L
    var jd = (4L * jd1 - 1L) % 146097L
    var t = jd / 4L
    jd = (4L * t + 3L) / 1461L
    t = (((4L * t + 3L) % 1461L) + 4L) / 4L
    val m1 = (5L * t - 3L) / 153L
    val j1 = (100L * j + jd).toInt
    if (m1 < 10L) j1 else j1 + 1
  }

  def decode(text: String): (Expression, Int) = {
    val end = CurrencyConstant.findEnd(text, 2)
    (DateConstant(text.substring(2, end).toLong), end)
  }

  def apply(l: Long): DateConstant = asJulian(l)

  def getDaysInMonth(month: Int, year: Int): Int = month match {
    case 4 | 6 | 9 | 11 => 30
    case 2 => if (leapYear(year) == 1) 29 else 28
    case _ => 31
  }

  def fromWeekNr(weekNr: Int, year: Int): DateConstant = {
    val firstThursday = DateConstant.julian(4, 1, year) + 3 - DateConstant.getWeekDayInYear(year, 4)
    asJulian(firstThursday - 3 + (weekNr - 1) * 7)
  }

  def getWeekDayInYear(year: Int, n: Int): Int = {
    val j = (year - 1) % 100
    val c = (year - 1) / 100
    (((28 + j + n + (j / 4) + (c / 4) + 5 * c) % 7) + 6) % 7
  }

  def julian(day: Int, month: Int, year: Int): Long = {
    val m: Long = if (month > 2) month - 3 else month + 9
    val j: Long = if (month > 2) year else year - 1
    (146097L * (j / 100L)) / 4L + (1461L * (j % 100L)) / 4L + (day + (153 * m + 2) / 5) + 1721119L
  }

  def getEaster(year: Int): DateConstant = {
    val n = easter(year)
    DateConstant(dayInYear(year, n), monthInYear(year, n), year)
  }

  /** gets the day in year of easter
    *
    * @param year the year
    * @return the day number in the given year when its easter
    */
  def easter(year: Int): Int = {
    val gz = (year % 19) + 1
    val jhd = year / 100 + 1
    val ksj = (3 * jhd) / 4 - 12
    val korr = (8 * jhd + 5) / 25 - 5
    val so = (5 * year) / 4 - ksj - 10
    val epakte = (11 * gz + 20 + korr - ksj) % 30
    val epakte1 = if ((epakte == 25 && gz > 11) || epakte == 24)
                    epakte + 1 else epakte
    val n = 44 - epakte1
    val n1 = if (n < 21) n + 30 else n
    (n1 + 7 - (so + n1) % 7) + leapYear(year) + 59
  }

  def monthInYear(year: Int, dayNum: Int): Int = {
    val a = leapYear(year)
    val n1 = if (dayNum > 59 + a) dayNum + 93 - a
             else dayNum + 91
    (20 * n1) / 611 - 2
  }

  def dayInYear(year: Int, dayNum: Int): Int = {
    val a = leapYear(year)
    val n1 = if (dayNum > 59 + a) dayNum + 93 - a
             else dayNum + 91
    n1 - (611 * ((20 * n1) / 611)) / 20
  }

  def leapYear(year: Int): Int = if (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) 1
                                 else 0

  /*def dayInYear(year:Int,dayNum:Int)= {
    val a = leapYear(year)
    var n=dayNum
    if ( n > 59+a ) n += 2-a
    n += 91
    val m = (20*n)/611
    n - (611*m)/20
  }*/

  private[expression] def apply(file: DataInput): DateConstant = from1970Millis(file.readLong)

  def from1970Millis(millis: Long): DateConstant = asJulian(julian1970 + millis / 86400000L)


}