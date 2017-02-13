package definition.expression

import definition.typ.DataType
import java.io.DataOutput
import java.io.DataInput

@SerialVersionUID(14237L) case class Variable(module:String,name:String) extends Expression {

  def getType =  DataType.VariableTyp 

  def getValue(): Constant = if(FunctionManager.get==null) EMPTY_EX else FunctionManager.get.getVariableValue(module,name) 

  def createCopy(): Expression = new Variable(module,name)

  def getChildCount: Int =  0 

  def getChildNr(ix: Int): Expression =  null 

  def getTerm: String = if(module.length==0) name else module + "_" + name

  def isConstant: Boolean =  false 

  def write(file:DataOutput)= { 
  	file.writeByte(DataType.VariableTyp.id)  	
  	file.writeUTF(module)
  	file.writeUTF(name)  	  	
  }

  def encode: String = "$Y"+getTerm+";"

}

object Variable {
  private [expression] def apply (file: DataInput):Expression = Variable(file.readUTF,file.readUTF)
  
  def decode(text:String):(Expression,Int) = {
    val pos=text.indexOf(';',5)    
    val sub=text.substring(2,pos).split('_')
    if(sub.length!=2) throw new IllegalArgumentException("Cant decode Variable '"+text+"'")
    (Variable(sub(0),sub(1)),pos+1)
  }
}