/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import definition.typ.DataType
import java.io.{DataInput,DataOutput}

/** Binar Operation expression
 * 
 */
@SerialVersionUID(14279L) case class BinaryOperation(left:Expression,operator:BinOperator,right:Expression) extends Expression {
  //System.out.println("create BinOp "+left+":"+left.getType+" and "+right+":"+right.getType+" "+operator)
  def getType: DataType.Value = { DataType.BinOp  }  

  def getValue: Constant = {
  	val value1=left.getValue
  	val value2=right.getValue
  	if(value1==null)throw new IllegalArgumentException("NULLvalue left:"+left+" "+left.getTerm)
  	operator.getValue(value1,value2)  match { 
  		case Some(value) => value
  		case None => throw new IllegalArgumentException("BinaryOperation no result  value1 "+value1+" "+value1.getType+" value2:"+
  		value2+" "+value2.getType)
  	}
  } 
  

  def createCopy(): Expression = { new BinaryOperation(left.createCopy(),operator,right.createCopy()) }

  def getChildCount: Int =  2 

  def getChildNr(ix: Int): Expression = { ix match {case 0 => left;case 1=>right }}
  
  private def checkParentheses(arg:Expression,right:Boolean):String =  {
     arg match {
    	 case bi:BinaryOperation if bi.operator.level<operator.level||
    	     (right&&(bi.operator.level==operator.level)&&(operator.opChar=='/'||operator.opChar=='%'||operator.opChar=='-') ) =>  "("+arg.getTerm+")"
    	 case _ => arg.getTerm
     }
  }

  def getTerm: String =   checkParentheses(left,false) + " "+operator+" "+checkParentheses(right,true)

  def isConstant: Boolean =  false 
  
  def write(file:DataOutput)= { 
  	file.writeByte(DataType.BinOp.id)
  	file.writeChar(operator.opChar )
  	left.write(file)
  	right.write(file)
  }
  
    
  override def getElementList[T <: Expression](whatType:DataType.Value,resultList:List[T]):List[T]={
  	left.getElementList(whatType,right.getElementList(whatType,super.getElementList(whatType,resultList)))		
	}
  
  
  
  override def replaceExpression(checker:(Expression) => Expression): Expression =  {
    val newMe=checker(this)
    if (newMe==this) // was not changed    	
    	new BinaryOperation(left.replaceExpression(checker),operator,right.replaceExpression(checker))    
    else newMe // return replacement of me
  }
  
  def encode:String = {
    "$B"+operator.toString+"("+left.encode+","+right.encode+")"
  }
  
  
}

object BinaryOperation
{
	private [expression] def apply (file: DataInput):Expression = {
		val op=file.readChar()
		new BinaryOperation	(Expression.read(file),BinOperator.getOp(op),Expression.read(file))
	}
	
	def decode(text:String):(Expression,Int) = {
		val op=BinOperator.getOp(text.charAt(2))
		val firstPar=Expression.decode(text.substring(4,text.length))
		val secPar=Expression.decode(text.substring(5+firstPar._2,text.length))
		(new BinaryOperation(firstPar._1,op,secPar._1),6+firstPar._2+secPar._2)
	}
}