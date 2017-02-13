/**
 * Author: Peter Started:14.08.2010
 */
package definition.expression

import java.io.DataInput
import java.io.DataOutput

import definition.typ.DataType
/** a Function call that collects data from all child objects
 * of the instance where this expression is stored
 * @param name name of the function
 * @param propertyField what property Field should be summed up 
 * @param childType the type id of the  children where data should be collected
 * @param childField from what fild of the children should data be collected
 * @param cacheValue optional List of all values of all children fields 
 */
case class CollectingFuncCall(name:String,propertyField:Byte,childType:Int,childField:Byte,
	cacheValue:Option[Constant]=None) extends Expression {

	def getType: DataType.Value = { DataType.CollFunctionCall }

	def getValue(): Constant = cacheValue.getOrElse(EMPTY_EX)


	def createCopy(): Expression = { new CollectingFuncCall(name,propertyField,childType,childField,cacheValue) }

	def getChildCount: Int =  0 

	def getChildNr(ix: Int): Expression =  null 

	def getTerm: String = "#"+ name+ "( "+propertyField+";"+childType+";"+childField +" ) " 

	def isConstant: Boolean =  false 

	def write(file:DataOutput)= { 
		file.writeByte(DataType.CollFunctionCall.id)  	
		file.writeUTF(name)
		file.writeByte(propertyField)
		file.writeInt(childType)  	
		file.writeByte(childField)  	 		
    cacheValue match {
      case Some(cv)=>file.writeBoolean(true);cv.write(file)
      case None => file.writeBoolean(false)
    }
	}
	/** creates a copy of this object with cacheValue set to the new value
	 * 
	 * @param newValue the new value
	 * @return a copy of this instance
	 */
	def setValue(newValue:Constant) = new CollectingFuncCall(name,propertyField,childType,childField,Some(newValue))
	
	override def equals(other: Any): Boolean =
	other match {
		case that: CollectingFuncCall =>
		name == that.name &&
		propertyField == that.propertyField &&
		childType == that.childType &&
		childField == that.childField
		case _ => false
	}

	override def hashCode: Int = 41 * ( 41 + name.hashCode)  + 1013*(3+propertyField.hashCode)+childType.hashCode + 45000*(2+childField.hashCode)
	
	def encode = {
	  "$S("+name+";"+propertyField+";"+childType+";"+childField +")"
	}
}  

object CollectingFuncCall
{
	private [expression] def apply (file: DataInput):Expression = {
		new CollectingFuncCall(file.readUTF,file.readByte,file.readInt,file.readByte, {
			if(file.readBoolean) Some(Expression.read(file).asInstanceOf[Constant])
			else None
		})
	}
	
	def decode(text:String)= {
	  val endPos=text.indexOf(')')
	  val parts=text.substring(3,endPos).split(';')	  
	  (new CollectingFuncCall(parts(0),parts(1).toByte,parts(2).toInt,parts(3).toByte),endPos+1)
	}
}

