/**
  * Author: Peter Started:21.09.2010
  */
package definition.typ

import java.io.DataInput

/**
  *
  */
case class AnswerDefinition(name: String, dataType: DataType.Value, followQuestion: Option[ParamQuestion],
                            constraint: String = "") {

  /*def write(out:DataOutput)= {
    out.writeUTF(name)
    out.writeInt(dataType.id)
    out.writeUTF(constraint)
    followQuestion match {
      case Some(question) =>out.writeBoolean(true); question.write(out)
      case None => out.writeBoolean(false)
    }

  }*/
  def toXML: scala.xml.Node = {
    <Answer typ={dataType.id.toString} name={name} con={constraint}>
      {followQuestion match {
      case Some(d) => d.toXML
      case None =>
    }}
    </Answer>
  }

  override def toString: String = "(AnswerDef " + name + " " + dataType + " followQuestion:" + followQuestion + ")"

}


object AnswerDefinition {

  val NonNullConstraint = "nonull"

  def fromXML(node: scala.xml.Node): AnswerDefinition = {
    val dtype = (node \ "@typ").text.toInt
    val const = (node \ "@con").text
    val question = ParamQuestion.fromXML(node)
    new AnswerDefinition((node \ "@name").text, DataType(dtype), question, const)
  }

  def fromStream(in: DataInput): AnswerDefinition = {
    val name = in.readUTF()
    val dt = DataType(in.readInt())
    val q = if (in.readBoolean()) Some(ParamQuestion.fromStream(in)) else None
    val constraint = in.readUTF
    new AnswerDefinition(name, dt, q, constraint)
  }

  /*def apply(in:DataInput):ParamAnswerDefinition = {
    new ParamAnswerDefinition(in.readUTF,DataType(in.readInt),if(in.readBoolean)Some(ParamQuestion(in)) else None,in.readUTF)
  }*/
}


	
	
