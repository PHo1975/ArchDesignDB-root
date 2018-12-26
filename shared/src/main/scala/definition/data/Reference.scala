/**
  * Author: Peter Started:25.07.2010
  */
package definition.data

import java.io.{DataInput, DataOutput}

import definition.typ.AllClasses
import util.Log

import scala.util.control.NonFatal
import scala.util.matching.Regex

/** a reference to an instance
  *
  */
@SerialVersionUID(24276L) case class Reference(typ: Int, instance: Int) extends Referencable // with Serializable
{
  def sToString(): String = "(" + typ + "," + instance + ")"

  def lToString(): String = "(" + AllClasses.get.getClassByID(typ).name + " #" + instance + ")"

  def bToString(): String = typ.toString + "," + instance

  override def write(file: DataOutput): Unit = {
    file.writeInt(typ)
    file.writeInt(instance)
  }

  def isNull: Boolean = typ == 0 && instance == 0

  // def serialized= new SerialReference(typ,instance)
  def ref: Reference = this

  def compareTo(other: Reference): Int = if (typ < other.typ) -1 else if (typ > other.typ) 1 else if (instance < other.instance) -1 else if (instance > other.instance) 1 else 0
}


object Reference {
  lazy val ordering: Ordering[Reference] = (a: Reference, b: Reference) => a.compareTo(b)
  val RMatch: Regex ="""\(?(\d+)[,\.](\d+)\)?""".r

  def apply(file: DataInput): Reference = {
    new Reference(file.readInt, file.readInt)
  }

  def apply(str: String): Reference = str match {
    case RMatch(typ, inst) => new Reference(typ.toInt, inst.toInt)
    case _ => EMPTY_REFERENCE
  }

  def unapply(str: String): Option[Reference] = str match {
    case RMatch(typ, inst) => Some(new Reference(typ.toInt, inst.toInt))
    case _ => None
  }

}


object EMPTY_REFERENCE extends Reference(0, 0)


object RefList {
  def unapply(st: String): Option[Array[Reference]] = try {
    Some(st.split(';').collect { case Reference(a) => a })
  }
  catch {
    case NonFatal(e) => Log.e("Error decoding ", e); None
  }
}

