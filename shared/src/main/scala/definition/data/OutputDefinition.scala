/**
  * Author: Peter Started:22.12.2010
  */
package definition.data

import definition.expression._
import definition.typ.{DataType, SystemSettings}

import scala.xml.Elem

case class ResultElement(paramName:String,result:Constant)

/**
  *
  */
case class OutputDefinition(odInst: Int, formInst: Int, printer: String, paperSetting: String, portrait: Boolean, paramValues: Iterable[ResultElement]) {
  lazy val outName: String = getOutName
  var formName: String = ""

  def getOutName: String = {
    val papers = paperSetting.split('|')
    //println("Papers "+papers.mkString("**")+ "\n"+ papers(0)+ " - " +papers(1))
    val paperText = if (papers.length > 1) formPaper(papers(0)) + " in " +
      (if (OutputDefinition.trayTranslations.contains(papers(1))) OutputDefinition.trayTranslations(papers(1)) else papers(1))
                    else formPaper(paperSetting)
    (if (formName.length == 0) "Form " + formInst else formName) + " mit " + printer.split(" ").head + " auf Papier " +
      paperText + " | " + (if (portrait) " Hochformat" else " Querformat")
  }

  def formPaper(s: String): String = s.replace("iso-", "").toUpperCase

  def toXML: Elem = <OutDef odInst={odInst.toString} formInst={formInst.toString} printer={printer} paper={paperSetting} portrait={if (portrait) "1" else "0"}>
    {paramValues.map(paramToXML)}
  </OutDef>

  def paramToXML(p: ResultElement): Elem = <PValue name={p.paramName} value={p.result.getTerm}/>

  override def toString: String = outName

  def paramString: String = paramValues.map { case ResultElement(pname, pvalue) =>
    if (pvalue.getType == DataType.BoolTyp) (if (pvalue.toBoolean) "mit " else "ohne ") + pname
    else if (pvalue.isNullConstant) "" else pname + "=" + pvalue
  }.mkString("; ")
}


object OutputDefinition {
  val trayTranslations: Map[String, String] = Map("bottom" -> "Unten", "top" -> "Oben", "manual" -> "Manuell", "Automatic-Feeder" -> "Autom. Einzug")
  val odefType: Int = SystemSettings().systemTypes("OutputDef")

  def apply(data: InstanceData, paramChildren: Seq[InstanceData]): OutputDefinition = {
    if (data.ref.typ != odefType) throw new IllegalArgumentException("Instance " + data + " is not an Outputdef")
    val paramList = paramChildren.map(c =>ResultElement(c.fieldValue.head.toString, c.fieldValue(1)))
    //println("outputdef paramList types :"+paramList.map(_._2.getType))
    new OutputDefinition(data.ref.instance, data.fieldValue.head.toInt, data.fieldValue(1).toString, data.fieldValue(2).toString, data.fieldValue(3).toBoolean,
      paramList)
  }

  def fromXML(node: scala.xml.Node): OutputDefinition = {
    new OutputDefinition((node \ "@odInst").text.toInt, (node \ "@formInst").text.toInt, (node \ "@printer").text, (node \ "@paper").text,
      (node \ "@portrait").text == "1", (node \\ "PValue").map(paramFromXML))
  }

  def paramFromXML(node: scala.xml.Node): ResultElement ={
    val text=(node \ "@value").text.replace("\"", "")
    ResultElement((node \ "@name").text, (StringParser.parse(text) match {
      case ex: Expression => ex
      case _ : ParserError =>  StringConstant(text)
    }).getValue)
  }

}