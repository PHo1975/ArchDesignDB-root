/**
 * Author: Peter Started:18.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataOutput}

/**
 * 
 */
case class DoubleConstant(n:Double) extends Constant {
  
  def getType: DataType.Value =  DataType.DoubleTyp 

  def createCopy(): Expression = { new DoubleConstant(n) }

  def getTerm =   toString
  
  override def toString=String.valueOf(n)  

  def toInt =  n.round.toInt
  
  def toLong =  n.round
  
  def toDouble = n
  
  def toBoolean= n>0
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.DoubleTyp.id)
  	//System.out.println("DoubleID:" +DataType.DoubleTyp.id)
  	file.writeDouble(n)
  }
  
  def getNative= n
  
  override def isNumberConstant=true
  
  def encode="$G"+n.toString()
}

object DoubleConstant {
  //val format=new java.text.DecimalFormat("0.0########")
  def decode(text:String)= {
    val end=findEnd(text,2)
	(new DoubleConstant(text.substring(2,end).toDouble),end)
  }
  
  val allowedChars:Array[Char]=Array('0','1','2','3','4','5','6','7','8','9','.','-')
	
	def findEnd(text:String,startPos:Int)= {
	  var pos=startPos
	  while(pos<text.length && allowedChars.contains( text.charAt(pos)))
	    pos+=1
	  pos
	}
}


