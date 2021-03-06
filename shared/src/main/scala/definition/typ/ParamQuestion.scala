/**
  * Author: Peter Started:21.09.2010
  */
package definition.typ

import java.io.DataInput

import definition.data.Referencable
import util.{StoreClassInfo, StringUtils}


/** Questions that Actions ask for to get their parameters
  *
  */

trait ParamQuestion extends StoreClassInfo {
  def toXML: scala.xml.Node

  def repeat: Boolean
}


case class DialogQuestion(name: String, possibleAnswers: Seq[AnswerDefinition], repeat: Boolean = false, visualOption: String = "") extends ParamQuestion {
  def toXML: scala.xml.Node = {
    <DialogQuestion name={name} repeat={if (repeat) "1" else "0"} visual={visualOption}>
      {for (ans <- possibleAnswers) yield ans.toXML}
    </DialogQuestion>
  }

  override def toString: String = "Question " + name + " answers=" + possibleAnswers + " repeat:" + repeat + " visual:" + visualOption

  def classID = 1
}


trait CustomPanel {
  def open(): Unit

  def load(groups: Iterable[SelectGroup[_ <: Referencable]]): Boolean = {true}

  def setFocus(): Unit

  def shutDown(): Unit

  def name: String
}


trait PanelQuestion extends ParamQuestion {
  def repeat = false

  def panel: CustomPanel

  def name: String
}


case class PanelRemoteQuestion(panelClass: String) extends PanelQuestion {
  lazy val panel: CustomPanel = Class.forName(panelClass).getConstructor().newInstance().asInstanceOf[CustomPanel]

  def toXML: scala.xml.Node = {<PanelQuestion name={panelClass}></PanelQuestion>}

  def name: String = panel.name

  def classID = 2
}

object ModuleType extends Enumeration {
  val Graph=Value("Graph")
  val Plot=Value("Plot")
  val Print=Value("Print")
}

case class CommandQuestion(module: ModuleType.Value, commandName: String) extends ParamQuestion {
  def repeat = false

  def classID = 3

  def toXML: scala.xml.Node = {
      <CommandQuestion module={module.id.toString} command={commandName}/>
  }
}


case class XMLQuestion(module: ModuleType.Value, customData: Seq[scala.xml.Node]) extends ParamQuestion {
  def toXML: scala.xml.Node = {
    <XMLQuestion moduleName={module.id.toString}>
      {customData}
    </XMLQuestion>
  }

  def repeat = false

  def classID = 4
}


object ParamQuestion {

  def fromXML(superNode: scala.xml.Node): Option[ParamQuestion] = {
    val dnode = superNode \\ "DialogQuestion"
    if (dnode.isEmpty) {
      val dnode = superNode \\ "XMLQuestion"
      if (dnode.isEmpty) {
        val dnode = superNode \\ "PanelQuestion"
        if (dnode.isEmpty) {
          val dnode = superNode \\ "CommandQuestion"
          if (dnode.isEmpty) None
          else Some(CommandQuestion(ModuleType((dnode \ "@module").text.toInt), (dnode \ "@command").text))
        }
        else Some(PanelRemoteQuestion((dnode \ "@name").text))
      }
      else {
        val moduleText=(dnode \ "@module").text
        Some(XMLQuestion(if(moduleText.trim.size==0)ModuleType.Print else ModuleType(StringUtils.stringToInt(moduleText)), dnode.head.child))
      }
    }
    else {
      val subNode = dnode.head
      val name = (subNode \ "@name").text
      val repeat = (subNode \ "@repeat").text == "1"
      val visual = (subNode \ "@visual").text
      val answers = for (afield <- subNode \ "Answer") yield AnswerDefinition.fromXML(afield)
      Some(DialogQuestion(name, answers, repeat, visual))
    }
  }

  def fromStream(in: DataInput): ParamQuestion = {
    in.readInt() match {
      case 1 => DialogQuestion(in.readUTF, for (_ <- 0 until in.readInt) yield AnswerDefinition.fromStream(in), in.readBoolean(), in.readUTF())
      case 2 => PanelRemoteQuestion(in.readUTF)
      case 3 => CommandQuestion(ModuleType(in.readInt), in.readUTF)
      case 4 => XMLQuestion(ModuleType(in.readInt), null)
    }
  }

  def makeQuestion(parms: List[(String, String, DataType.Value)]): Option[ParamQuestion] = {
    val firstQ = parms.head
    Some(DialogQuestion(firstQ._1, Seq(
      new AnswerDefinition(firstQ._2, firstQ._3, if (parms.tail.isEmpty) None else makeQuestion(parms.tail)))))
  }

  //
  def makeQuestionWithConstraints(parms: List[(String, String, DataType.Value, String)]): Option[ParamQuestion] = {
    val firstQ = parms.head
    Some(DialogQuestion(firstQ._1, Seq(
      new AnswerDefinition(firstQ._2, firstQ._3, if (parms.tail.isEmpty) None else makeQuestionWithConstraints(parms.tail), firstQ._4))))
  }
}

