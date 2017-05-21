/**
  * Author: Peter Started:28.07.2010
  */
package definition.typ

import java.io.{ DataInput, DataOutput }
import util.XMLUtils._
import util.{ CustomSerializer, StrToInt }
import scala.xml.{ Elem, Text }

trait AbstractCCD extends CustomSerializer {
  lazy val childName: String = AllClasses.get.getClassByID(childClassID).getDescriptionOrName

  def editorName: String

  def childClassID: Int

  def action: Option[ActionTrait]

  def toXML: Elem = {
    <CC cID={childClassID.toString} e={optText(editorName)}>
      {action match {
      case Some(a) => a.toXML
      case _ =>
    }}
    </CC>
  }

  def getName: String = action match {
    case Some(action) => action.name
    case _ => "*"
  }

  def toXMLFile: Elem = {
      <CC cID={childClassID.toString} e={optText(editorName)} actionName={action map (a => Text(a.name))}/>
  }

  def writeToStream(out: DataOutput): Unit = {
    out.writeUTF(editorName)
    out.writeInt(childClassID)
    action match {
      case Some(a) => out.writeBoolean(true); a.writeToStream(out)
      case None => out.writeBoolean(false)
    }
  }

  override def toString: String = "CCD editorName:" + editorName + " childClass:" + childClassID + " action:" + action
}


/** defines a createAction to create a certain child type in that propertyField
  *
  */
case class CreateChildDefinition(editorName: String = "", childClassID: Int = 0, action: Option[ActionTrait]) extends AbstractCCD


object CreateChildDefinition {

  def fromXML(node: scala.xml.Node): CreateChildDefinition = {
    val editor = readOptString(node, "@e")
    val childClass = (node \ "@cID").text match {
      case StrToInt(i) => i
      case _ => 0
    }
    val action = (for (afield <- node \\ "Action") yield ActionDescription.fromXML(afield)).headOption
    //}
    CreateChildDefinition(editor, childClass, action)
  }

  def fromStream(in: DataInput) =
    CreateChildDefinition(in.readUTF(), in.readInt(), if (in.readBoolean) Some(ActionDescription.fromStream(in)) else None)
}


/** defines a property field in a class
  *
  */
case class PropertyFieldDefinition(name: String, single: Boolean = false, allowedClass: Int = 0,
                                   createChildDefs: Seq[AbstractCCD] = Seq.empty, hidden: Boolean = false, volatile: Boolean = false) {

  def toXML(toFile: Boolean): Elem = {
    <PropertyFD name={name} single={boolText(single)} allowedClass={optInt(allowedClass)} hidden={boolText(hidden)} volatile={boolText(volatile)}>
      {if (toFile) createChildDefs.map(_.toXMLFile) else createChildDefs.map(_.toXML)}
    </PropertyFD>
  }

  def setName(newName: String) = new PropertyFieldDefinition(newName, single, allowedClass, createChildDefs)

  def setSingle(newValue: Boolean) = new PropertyFieldDefinition(name, newValue, allowedClass, createChildDefs)

  def setAllowedClass(newValue: Int) = new PropertyFieldDefinition(name, single, newValue, createChildDefs)

  def setChildDefs(newList: Seq[AbstractCCD]) = new PropertyFieldDefinition(name, single, allowedClass, newList)

  def setHidden(newValue: Boolean) = new PropertyFieldDefinition(name, single, allowedClass, createChildDefs, newValue)

  def setVolatile(newValue: Boolean) = new PropertyFieldDefinition(name, single, allowedClass, createChildDefs, hidden, newValue)
}


object PropertyFieldDefinition {
  def fromXML(node: scala.xml.Node, childDefFactory: (scala.xml.Node) => AbstractCCD): PropertyFieldDefinition = {
    val name = (node \ "@name").text
    val single = readBool(node, "@single")
    val allowedClass = readOptString(node, "@allowedClass") match {
      case StrToInt(i) => i
      case _ => 0
    }
    val createChildList = for (afield <- node \\ "CC") yield childDefFactory(afield)
    PropertyFieldDefinition(name, single, allowedClass, createChildList, readBool(node, "@hidden"), readBool(node, "@volatile"))
  }

  def fromStream(in: DataInput): PropertyFieldDefinition = {
    val name = in.readUTF()
    val single = in.readBoolean()
    val allowedClass = in.readInt()
    val numCCD = in.readInt()
    //if(numCCD>0) print("propfield "+name+" allowed Class:"+allowedClass+" ccd:"+numCCD+" ")
    val ccd = for (_ <- 0 until numCCD) yield CreateChildDefinition.fromStream(in)
    PropertyFieldDefinition(name, single, allowedClass, ccd, in.readBoolean(), in.readBoolean())
  }

}