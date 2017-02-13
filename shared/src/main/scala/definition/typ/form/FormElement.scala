/**
 * Author: Peter Started:13.04.2011
 */
package definition.typ.form

import java.awt.Color

import definition.data.InstanceData
import definition.expression.{Expression, ParserResult}
import definition.typ.AbstractObjectClass

import scala.language.implicitConversions
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.{Attribute, MetaData, Null, Text}



/** Super class of all screen form elements
 * 
 */
trait AbstractFormElement {
   def minWidth:Int
   def maxWidth:Int
   def minHeight:Int
   def maxHeight:Int 
   def toXML:scala.xml.Node   
   
   type InitFunc=(AbstractFormElement)=>Unit

   
   def getProps:List[(String,String)]= {
  	 val node=this.toXML
  	 node.attributes.map(a=> (a.key,a.value.text)).toList.sortWith((a,b)=> a._1 < b._1)
  	 //node.attributes
   }
   
   protected def changeAttribute(attr:MetaData,key:String,newValue:String):MetaData= {
  	 if(attr==Null) return attr
  	 if(attr.key.equalsIgnoreCase(key)) Attribute(attr.key,Text(newValue),changeAttribute(attr.next,key,newValue))
  	 else Attribute(attr.key,attr.value,changeAttribute(attr.next,key,newValue))
   }
   
   protected implicit def iterableToMetaData(items: Iterable[MetaData]): MetaData = {
  	 items match {
  		 case Nil => null
  		 case head :: tail => head.copy(next=iterableToMetaData(tail))
  	}    
   }
   
   def select(selCol:Color):Unit
   
   def deselect(): Unit
  
   def makeCopy: AbstractFormElement
}

/** Interface for an mechanism to handle updates of fields 
 * 
 */
trait DataChangeListener { 
	def fieldChanged(field:Byte,newValue:Expression)
	def parseValue(fieldNr:Byte,text:String):ParserResult
	def flipMaximizeWindow(max:Boolean):Unit
	def print():Unit
}

trait FormDataField extends AbstractFormElement {
	var listener:Option[DataChangeListener]=None
	def fieldNr:Byte
	def wantShutDown():Unit
	def shutDown():Unit
	def setDataValue(dvalue:InstanceData,nclass:AbstractObjectClass)
	def setListener (nlist:Option[DataChangeListener])= {
		listener=nlist
	}		
}


	
