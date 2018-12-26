/**
  * Author: Peter Started:21.09.2010
  */
package definition.typ

import java.io.{DataInput, DataOutput}

import util.{CollUtils, CustomSerializer, StrToInt}

/** describes an Action of a Class
  *
  */

trait ActionTrait extends Product with CustomSerializer {
  def name: String

  def isIterator: Boolean

  def rebound: Boolean

  def buttonID: Int

  def hiddenFromPanel = false // If hidden, the Action description will not be send via XML to the client

  def toXML: scala.xml.Elem

  override def toString: String = "Action [" + (if (name == null) "null" else name) + "] question:" + question //+(if(repeat)"  arepeat" else "")

  def question: Option[ParamQuestion] = None

  def writeToStream(out: DataOutput): Unit = {
    out.writeUTF(name)
    question match {
      case Some(pq) => out.writeBoolean(true); CollUtils.writePrimitive(out, pq)
      case None => out.writeBoolean(false)
    }
    out.writeBoolean(isIterator)
    out.writeBoolean(rebound)
    out.writeInt(buttonID)
  }

  def productArity = 5

  override def productIterator: scala.Iterator[scala.Any] = (0 until 5).iterator.map(productElement)

  def productElement(n: scala.Int): Any = n match {
    case 0 => name
    case 1 => question
    case 2 => isIterator
    case 3 => rebound
    case 4 => buttonID
    case _ => null
  }

  override def productPrefix: java.lang.String = "A"

  def canEqual(other: Any): Boolean = other.isInstanceOf[ActionTrait]
}


case class ActionDescription(name: String, override val question: Option[ParamQuestion], isIterator: Boolean, rebound: Boolean, buttonID: Int) extends ActionTrait {
  def toXML: scala.xml.Elem = throw new IllegalArgumentException("Cant run toXML in abstract ActionDescription")

  //def write(out:DataOutput)= {}
  override def toString: String = name + " question:" + question + " isIterator:" + isIterator + " rebound:" + rebound
}


object ActionDescription {

  def fromXML(node: scala.xml.Node) =
    new ActionDescription((node \ "@name").text, ParamQuestion.fromXML(node), (node \ "@iter").text == "1",
      (node \ "@reb").text == "1", (node \ "@id").text match {
        case StrToInt(id) => id
        case _ => 0
      })

  def fromStream(in: DataInput) =
    new ActionDescription(in.readUTF(), if (in.readBoolean()) Some(ParamQuestion.fromStream(in)) else None,
      in.readBoolean(), in.readBoolean(), in.readInt())

}