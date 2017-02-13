/**
 * Author: Peter Started:30.11.2010
 */
package definition.expression

import java.io.DataOutput
//import java.util.Locale

import definition.typ.DataType


/**
 * 
 */
case class CurrencyConstant(n:Long) extends Constant {
	lazy val doubleValue=n.toDouble/100d
  def getType: DataType.Value = { DataType.CurrencyTyp }
  

  def createCopy(): Expression = { new CurrencyConstant(n) }

  def getTerm =  "%.2f".format(doubleValue)+CurrencyConstant.currencySign
  
  override def toString="%,.2f".format(doubleValue)+CurrencyConstant.currencySign
  
  def toInt =  doubleValue.round.toInt
  
  def toLong =  doubleValue.round.toLong
  
  def toDouble = doubleValue
  
  def toBoolean= n>0
  
  override def toUnitNumber= new UnitNumber(n.toDouble/100d,new UnitFraction(UnitNumber.setFactory+new UnitElem(CurrencyConstant.currencySign,1),UnitNumber.emptySet))
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.CurrencyTyp.id)
  	//System.out.println("DoubleID:" +DataType.DoubleTyp.id)
  	file.writeLong(n)
  }
  
  def getNative= doubleValue
  
  override def toCurrency=this  
  
  override def isNumberConstant=true 
  
  def encode="$C"+n.toString
}

//object ImBroke extends CurrencyConstant(0)

object CurrencyConstant {
	lazy val currencySign="€"
	
	def decode(text:String):(Expression,Int)= {
	  val end=findEnd(text,2)
	    (new CurrencyConstant(text.substring(2,end).toLong),end)
	}
	
	val allowedChars:Array[Char]=Array('0','1','2','3','4','5','6','7','8','9','-')
	
	def findEnd(text:String,startPos:Int)= {
	  var pos=startPos
	  while(pos<text.length && allowedChars.contains( text.charAt(pos)))
	    pos+=1
	  pos
	}
}