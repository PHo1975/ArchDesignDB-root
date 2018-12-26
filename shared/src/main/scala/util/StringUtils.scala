package util

import java.awt.font.TextLayout
import java.awt.geom.Rectangle2D
import java.awt.{Color, Graphics2D}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

object StringUtils {
  val __OBFUSCATE = "OBF:"

  def stringToFloat(st: String): Float = try {
    val tt = st.trim
    if (tt.length == 0) 0f
    else tt.replace(',', '.').toFloat
  } catch {case _: Exception => 0f}

  def stringToInt(st: String): Int = try {
    val tt = st.trim
    if (tt.length == 0) 0
    else tt.toInt
  } catch {case _: Exception => 0}

  def stringToDouble(st: String): Double = try {
    val tt = st.trim
    if (tt.length == 0) 0d
    else tt.replace(',', '.').toDouble
  } catch {case _: Exception => 0d}

  def splitBracketed(text: String, firstBrack: Char, secondBrack: Char): (Seq[String], Seq[String]) = {
    var textPos = 0
    val normalTexts = ArrayBuffer[String]()
    val subTexts = ArrayBuffer[String]()
    while (textPos < text.length) {
      val firstBrackPos = text.indexOf(firstBrack, textPos)
      if (firstBrackPos < 0) {normalTexts += text.substring(textPos, text.length); textPos = text.length}
      else {
        normalTexts += text.substring(textPos, firstBrackPos)
        val secondBrackPos = text.indexOf(secondBrack, firstBrackPos + 1)
        if (secondBrackPos < 0) {subTexts += text.substring(firstBrackPos + 1, text.length); textPos = text.length}
        else {
          subTexts += text.substring(firstBrackPos + 1, secondBrackPos)
          textPos = secondBrackPos + 1
        }
      }
    }
    (normalTexts, subTexts)
  }

  def mySgn(d: Double): Int = if (d < 0d) -1 else 1

  def fillTextLayout(g: Graphics2D, tl: TextLayout, xpos: Float, ypos: Float, wide: Boolean = false): Unit = {
    val tbound = tl.getBounds.asInstanceOf[Rectangle2D.Float]
    g.setPaint(Color.white)
    try {
      if (wide)
        g.fill(new Rectangle2D.Float(tbound.x + xpos - 2, tbound.y + ypos - 2, tbound.width + 4, tbound.height + 4))
      else
        g.fill(new Rectangle2D.Float(tbound.x + xpos - 1, tbound.y + ypos - 1, tbound.width + 2, tbound.height + 2))
    } catch {
      case NonFatal(e) => Log.e("filltext ", e)
    }
  }

  def encode(n: String): String = '"' + n.replace('"', '«') + '"'

  def decode(n: String): String = n.substring(1, n.length - 1).replace('«', '"')

  def escapeJSON(string: String): String =
    if (string == null || string.length() == 0) "\"\""
    else {
      val len = string.length()
      val sb = new StringBuilder(len + 4)
      sb.append('"')
      for (i <- 0 until len) {
        val c = string.charAt(i)
        c match {
          case '\\' | '"' => sb.append('\\'); sb.append(c)
          case '/' => sb.append('\\'); sb.append(c)
          case '\b' => sb.append("\\b")
          case '\t' => sb.append("\\t")
          case '\n' => sb.append("\\n")
          case '\f' => sb.append("\\f")
          case '\r' => sb.append("\\r")
          case '\u20ac' => sb.append("&euro;")
          case _ => if (c < ' ') {
            val t = "000" + Integer.toHexString(c)
            sb.append("\\u" + t.substring(t.length() - 4))
          } else sb.append(c)
        }
      }
      sb.append('"')
      sb.toString()
    }

  def obfuscate(s: String): String = {
    val buf = new StringBuffer()
    val b = s.getBytes
    buf.synchronized {
      buf.append(__OBFUSCATE)
      for (i <- b.indices) {
        val b1 = b(i)
        val b2 = b(s.length() - (i + 1))
        val i1 = 127 + b1 + b2
        val i2 = 127 + b1 - b2
        val i0 = i1 * 256 + i2
        val x = Integer.toString(i0, 36)

        x.length() match {
          case 1 => buf.append("000"); buf.append(x)
          case 2 => buf.append("00"); buf.append(x)
          case 3 => buf.append('0'); buf.append(x)
          case _ => buf.append(x)
        }
      }
      buf.toString
    }
  }

  /* ------------------------------------------------------------ */
  def deobfuscate(st: String): String = {
    val s = if (st.startsWith(__OBFUSCATE)) st.substring(4)
            else st
    val b = new Array[Byte](s.length() / 2)
    var l = 0
    var i = 0
    while (i < s.length()) {
      val x = s.substring(i, i + 4)
      val i0 = Integer.parseInt(x, 36)
      val i1 = i0 / 256
      val i2 = i0 % 256
      b(l) = ((i1 + i2 - 254) / 2).toByte
      l += 1
      i += 4
    }
    new String(b, 0, l)
  }
}


object StrToInt {
  def unapply(st: String): Option[Int] = try {
    val tt = st.trim
    Some(if (tt.length == 0) 0 else tt.toInt)
  } catch {case NonFatal(_) => None}
}


object StrToLong {
  def unapply(st: String): Option[Long] = try {
    val tt = st.trim
    Some(if (tt.length == 0) 0 else tt.toLong)
  } catch {case NonFatal(_) => None}
}


object StrToDouble {
  def unapply(st: String): Option[Double] = try {
    val tt = st.trim.replace(',', '.')
    Some(if (tt.length == 0) 0 else tt.toDouble)
  } catch {case NonFatal(_) => None}
}


object StrToFloat {
  def unapply(st: String): Option[Float] = try {
    val tt = st.trim.replace(',', '.')
    Some(if (tt.length == 0) 0 else tt.toFloat)
  } catch {case NonFatal(_) => None}
}


object SemicolonSplit {
  def unapply(st: String): Option[(String, String)] = {
    val splits = st.trim.split(';')
    if (splits.length != 2) None
    else Some((splits(0).trim, splits(1).trim))
  }
}


object SemicolonSplitList {
  def unapply(st: String): Option[Array[String]] = {
    val splits = st.trim.split(";")
    if (splits.isEmpty) None
    else Some(splits)
  }
}


object ColonSplit {
  def unapply(st: String): Option[(String, String)] = {
    val splits = st.trim.split(':')
    if (splits.length != 2) None
    else Some((splits(0).trim, splits(1).trim))
  }
}


object ColonSplitList {
  def unapply(st: String): Option[Array[String]] = {
    val splits = st.trim.split(':')
    if (splits.length == 0) None
    else Some(splits)
  }
}


object CommaSplit {
  def unapply(st: String): Option[(String, String)] = {
    val splits = st.trim.split(',')
    if (splits.length != 2) None
    else Some((splits(0).trim, splits(1).trim))
  }
}


object CommaSplitList {
  def unapply(st: String): Option[Array[String]] = {
    val splits = st.trim.split(',')
    if (splits.length == 0) None
    else Some(splits)
  }
}


object BarSplitList {
  def unapply(st: String): Option[Array[String]] = {
    val splits = st.trim.split('|')
    if (splits.length == 0) None
    else Some(splits)
  }
}

