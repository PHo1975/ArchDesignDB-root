package definition.data

import java.io.{DataInput, DataOutput}

import util.Log

import scala.util.control.NonFatal
import scala.util.matching.Regex

@SerialVersionUID(21543L) case class OwnerReference(ownerField: Byte, //in what property field of the owner instance
                                                    //is this instance stored
                                                    ownerRef: Reference) { // reference of the owner instance

  def this(of: Int, or: Reference) = this(of.toByte, or)

  def write(out: DataOutput): Unit = {
    out.writeByte(ownerField)
    ownerRef.write(out)
  }

  override def toString: String = (if (ownerRef == null) "()" else ownerRef.sToString()) + "|" + ownerField

  def sToString: String = ownerRef.bToString() + "," + ownerField

}


object OwnerReference {
  val RMatch: Regex ="""\(?(\d+)[,](\d+)[,](\d+)\)?""".r

  def read(in: DataInput) = new OwnerReference(in.readByte, Reference(in))

  def unapply(st: String): Option[OwnerReference] = try {
    st match {
      case RMatch(otyp, oinst, prField) => Some(new OwnerReference(prField.toByte, new Reference(otyp.toInt, oinst.toInt)))
      case _ => None
    }
  } catch {case NonFatal(_) => None}
}


object OwnerRefList {
  def unapply(st: String): Option[Array[OwnerReference]] = try {
    Some(st.split(';').collect { case OwnerReference(a) => a })
  }
  catch {
    case NonFatal(e) => Log.e("Error decoding  ", e); None
  }
}


object EMPTY_OWNERREF extends OwnerReference(0, EMPTY_REFERENCE)