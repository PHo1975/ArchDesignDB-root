package definition.expression

import definition.typ.DataType
import java.io.DataOutput

object TodayConstant extends Expression {
  val todayString="@heute"
  def getType=DataType.DateTyp
	
	def getValue()=generate.getValue
	
	def createCopy():Expression=this
	
	def getChildCount:Int=0
	
	def getChildNr(ix:Int):Expression=EMPTY_EX
	
	def getTerm:String=todayString
	
	def isConstant:Boolean=true	
	
	def write(file:DataOutput):Unit={    
  }	
	
	
	/** is called when generating an instance 
	 * will be overridden by generatorConstants like $Now and $Today
	 * 
	 * @return the actual state of that expression during generation time
	 */
	override def generate:Expression=DateConstant()
	
	def encode:String="$D"
	
	def decode(text:String):(Expression,Int)={
	  (this,2)
	}

}