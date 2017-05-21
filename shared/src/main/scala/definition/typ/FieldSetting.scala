/**
  * Author: Peter Started:20.11.2010
  */
package definition.typ

import java.io.DataInput
import definition.expression.{ EMPTY_EX, Expression }
import util.XMLUtils.{ boolText, optText, readBool, readOptString }
import scala.xml.{ Elem, Text }

/**
  *
  */
case class FieldSetting(fieldNr: Int, var readOnly: Boolean = false,
                        var visible: Boolean = true, var showFormula: Boolean = false, var editor: String = "", var startValue: Expression = EMPTY_EX,
                        var formString: String = "", var formatField: Boolean = false) {

  // formatField == this field will be used to format a graphical element in the graph editor
  override def toString: String =
    "FieldSetting(" + fieldNr + ") " + (if (readOnly) "ReadOnly" else "Writeable") + " " + (if (visible) "Visible" else "Hidden") +
      (if (showFormula) " showFormula " else "") + (if (editor.nonEmpty) " editor:" + editor else "") + " startValue:" + startValue + "formString:" + formString + "formField:" + formatField + "]"

  def toXML: Elem = {
      <FS n={fieldNr.toString} r={boolText(readOnly)}
          v={boolText(visible)}
          sh={boolText(showFormula)}
          ed={optText(editor)}
          st={if (startValue.isNullConstant) None else Some(Text(startValue.encode))}
          f={optText(formString)}
          ff={boolText(formatField)}/>
  }

  def setFieldNr(newFieldNr: Int) = new FieldSetting(newFieldNr, readOnly, visible, showFormula, editor, startValue, formString, formatField)

}


object EmptySetting extends FieldSetting(-1, false, true, false, "", EMPTY_EX)


object FieldSetting {

  def fromXML(node: scala.xml.Node): FieldSetting = {
    val fnr = (node \ "@n").text.toInt
    val readonly = readBool(node, "@r")
    val visible = readBool(node, "@v")
    val showForm = readBool(node, "@sh")
    val editor = readOptString(node, "@ed")
    val startV = readOptString(node, "@st")
    val startEx = if (startV == "") EMPTY_EX else Expression.decode(startV)._1
    val format = readOptString(node, "@f")
    val formatField = readBool(node, "@ff")
    //if(editor.size>0) System.out.println(" edit:"+name)

    FieldSetting(fnr, readonly, visible, showForm, editor, startEx, format, formatField)
  }

  def fromStream(in: DataInput) = FieldSetting(in.readInt(), in.readBoolean(), in.readBoolean(), in.readBoolean(), in.readUTF(),
    Expression.decode(in.readUTF)._1, in.readUTF(), in.readBoolean())
}