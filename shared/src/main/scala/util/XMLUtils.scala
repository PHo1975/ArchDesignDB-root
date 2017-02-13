package util

import scala.xml.Text

/**
 * Created by Kathi on 06.04.2015.
 */
object XMLUtils {
  def readBool(node: scala.xml.Node,fieldName:String):Boolean = {
    val nodeSeq=(node \ fieldName)
    if(nodeSeq==null) false
    else nodeSeq.text=="1"
  }

  def readOptString(node: scala.xml.Node,fieldName:String)= {
    val nodeSeq=(node \ fieldName)
    if(nodeSeq==null) "" else nodeSeq.text
  }

  def readOptInt(node: scala.xml.Node,fieldName:String)= {
    val nodeSeq=(node \ fieldName)
    if(nodeSeq==null) 0 else nodeSeq.text match {
      case StrToInt(i)=> i
      case _=> 0
    }
  }

  def optText(text:String)= if(text.length>0) Some(Text(text)) else None

  def boolText(value:Boolean)= if(value)Some(Text("1")) else None

  def optInt(value:Int)= if(value==0) None else Some(Text(value.toString))
}
