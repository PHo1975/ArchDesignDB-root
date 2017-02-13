/**
 * Author: Peter Started:23.07.2010
 */
package definition.expression

import definition.typ.DataType

/** Abstract trait of all operators for binary operations
 * 
 */

class  WrongUnitException(m:String) extends RuntimeException(m)

abstract class BinOperator(val opChar:Char,val level:Byte) extends Serializable {
	def getValue(left:Constant,right:Constant):Option[Constant]
	def getLongValue(left:Long,right:Long):Long
	def getDoubleValue(left:Double,right:Double):Double	
	def getCurrValue(left:Long,right:Long):Long
	def getUnitValue(left:UnitNumber,right:UnitNumber):UnitNumber
	
	private[expression] def getUnitSafeValue(left:Constant,right:Constant,keepFirstValueUnit:Boolean)= try {
	  getValue(left,right)
	} catch {
	  case e:WrongUnitException=>Some( new UnitNumber(getDoubleValue(left.toDouble,right.toDouble),(if(keepFirstValueUnit)left else right).toUnitNumber.unitFraction))
	}
	
	def doCalculation(c1:Constant,c2:Constant):Constant= { // constant 1 is Numberconstant
		if(c2.isNumberConstant) { // both are Number constants
		  c1 match {
		    case un:UnitNumber => getUnitValue(un,c2.toUnitNumber)
		    case cu1:CurrencyConstant=> c2 match {
		      case cu2:CurrencyConstant=> new CurrencyConstant(getCurrValue(cu1.n,cu2.n))
		      case un2:UnitNumber => getUnitValue(c1.toUnitNumber,un2)
		      case _=> new CurrencyConstant((getDoubleValue(cu1.toDouble,c2.toDouble)*100d).longValue)
		    }
		    case _=> c2 match {
		      case un:UnitNumber=> getUnitValue(c1.toUnitNumber,un)
		      case cu2:CurrencyConstant=> new CurrencyConstant((getDoubleValue(c1.toDouble,cu2.toDouble)*100d).longValue)		      
		      case _=>/* if(c1.getType==DataType.UnitNumberTyp||c2.getType==DataType.UnitNumberTyp)
		         getUnitValue(c1.toUnitNumber,c2.toUnitNumber)
		      else*/ if(c1.getType==DataType.DoubleTyp||c2.getType==DataType.DoubleTyp)
		      	new DoubleConstant(getDoubleValue(c1.toDouble,c2.toDouble))
		      else if(c1.getType==DataType.LongTyp||c2.getType==DataType.LongTyp)
		      	new LongConstant(getLongValue(c1.toLong,c2.toLong))
		      else new IntConstant(getLongValue(c1.toInt,c2.toInt).intValue)
		    } 
		  }			 
		} else c1.getType match {
		    		case DataType.IntTyp => new IntConstant (getLongValue(c1.toInt,c2.toInt).intValue)
		    		case DataType.LongTyp => new LongConstant (getLongValue(c1.toLong,c2.toLong).longValue)
		    		case DataType.DoubleTyp => new DoubleConstant (getDoubleValue(c1.toDouble,c2.toDouble))
		    		case DataType.CurrencyTyp => new CurrencyConstant ((getDoubleValue(c1.toDouble,c2.toDouble)*100d).longValue)
		    		case DataType.UnitNumberTyp=> getUnitValue(c1.toUnitNumber,c2.toUnitNumber)
		    	}	
	}	
	override def toString=opChar.toString
}

abstract class CompBinOperator(nopChar:Char) extends BinOperator(nopChar,0) {
  def getLongValue(left:Long,right:Long):Long=0
	def getDoubleValue(left:Double,right:Double):Double=0d
	def getCurrValue(left:Long,right:Long):Long=0
	def getUnitValue(left:UnitNumber,right:UnitNumber):UnitNumber=Expression.NULLUNITNUMBER
}

object BinOperator {
  val plusOp=new BinOperator('+',1){
		  def getLongValue(left:Long,right:Long):Long=left+right
			def getDoubleValue(left:Double,right:Double):Double=left+right
			def getCurrValue(left:Long,right:Long):Long=left+right
			def getUnitValue(left:UnitNumber,right:UnitNumber)=if(left.unitFraction.equals(right.unitFraction))
			  new UnitNumber(left.value+right.value,left.unitFraction)
			 else throw new WrongUnitException("+ Einheit '"+left+"' ist nicht kompatibel mit Einheit '"+right+"'" )
			def getValue(left:Constant,right:Constant)= {				
				if(left.isNumberConstant) Some(doCalculation(left,right))
				else left.getType match {
  				case DataType.StringTyp => Some(new StringConstant (left.toString + right.toString ))
  				case DataType.VectorTyp => if(right.getType==DataType.VectorTyp) {
  					Some(left.asInstanceOf[VectorConstant]+right.asInstanceOf[VectorConstant])  					
  				} else throw new IllegalArgumentException( "Cant add Datatype "+left.getType+" to Vectors ")
  				case DataType.undefined => Some(right)
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for add operation ")
  			} 			
			}
		}
  
  val minusOp=new BinOperator('-',1){
		  def getLongValue(left:Long,right:Long):Long=left-right
			def getDoubleValue(left:Double,right:Double):Double=left-right
			def getCurrValue(left:Long,right:Long):Long=left-right
			def getUnitValue(left:UnitNumber,right:UnitNumber)=if(left.unitFraction.equals(right.unitFraction))
			  new UnitNumber(left.value-right.value,left.unitFraction)
		  else throw new WrongUnitException("- Einheit '"+left+"' ist nicht kompatibel mit Einheit '"+right+"'" )
			def getValue(left:Constant,right:Constant)= {				
				if(left.isNumberConstant) Some(doCalculation(left,right))
				else left.getType match	{
  				case DataType.VectorTyp => if(right.getType==DataType.VectorTyp) {
  					Some(left.asInstanceOf[VectorConstant]-right.asInstanceOf[VectorConstant])
  				} else throw new IllegalArgumentException( "Cant add Datatype "+left.getType+" to Vectors ")
  				case DataType.undefined => Some(new DoubleConstant(0 - right.toDouble))
  				case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for sub operation ")
  			} 			
			}
		}
	
  
	/** finds the operator object with the given char
	 * 
	 * @param op name of the operator to find
	 */
	def getOp(op:Char) =
	{
		if (oList contains op) oList(op)
		else throw new IllegalArgumentException("Operator "+op+" is unknown")
	}
	
	
	def multString(st:String, num:Int)= {
		if(num<2)st else {
		val sb=	new StringBuilder()
			for(i<- 1 to num) {
			sb.append(st)
			}
		sb.toString()
		}			
	}
	
	private val oList=Map (
		'+' -> plusOp,
		'-' -> minusOp,
		'*' -> new BinOperator('*', 2) {
			def getLongValue(left: Long, right: Long): Long = left * right

			def getDoubleValue(left: Double, right: Double): Double = left * right

			def getCurrValue(left: Long, right: Long): Long = left * right / 100

			def getUnitValue(left: UnitNumber, right: UnitNumber) = new UnitNumber(left.value * right.value, left.unitFraction.mult(right.unitFraction))

			def getValue(left: Constant, right: Constant) = {
				if (left.isNumberConstant) Some(doCalculation(left, right))
				else
					left.getType match {
						case DataType.StringTyp => Some(new StringConstant(multString(left.toString, right.toInt)))
						case DataType.VectorTyp => {
							val leftVector = left.asInstanceOf[VectorConstant]
							if (right.getType == DataType.VectorTyp)
								Some(DoubleConstant(leftVector * right.asInstanceOf[VectorConstant]))
							else Some(leftVector * right.toDouble)
						}
						case DataType.undefined => Some(Expression.NullDOUBLE)
						case _ => throw new IllegalArgumentException("Wrong Datatype " + left.getType + " for mult operation " + left + " " + left.getTerm)
					}
			}
		},
		'/' -> new BinOperator('/', 2) {
			def getLongValue(left: Long, right: Long): Long = left / right

			def getDoubleValue(left: Double, right: Double): Double = left / right

			def getCurrValue(left: Long, right: Long): Long = left / right * 100

			def getUnitValue(left: UnitNumber, right: UnitNumber) = new UnitNumber(left.value / right.value, left.unitFraction.div(right.unitFraction))

			def getValue(left: Constant, right: Constant) = {
				if (left.isNumberConstant) Some(doCalculation(left, right))
				else
				/*left.getType match
  			{
					case DataType.undefined =>*/ Some(Expression.NullDOUBLE)
				/*case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for div operation ")
      } */

			}
		}
		,
		'%' -> new BinOperator('%', 2) {
			def getLongValue(left: Long, right: Long): Long = left % right

			def getDoubleValue(left: Double, right: Double): Double = left % right

			def getCurrValue(left: Long, right: Long): Long = left % right * 100

			def getUnitValue(left: UnitNumber, right: UnitNumber) = new UnitNumber(left.value % right.value, left.unitFraction.div(right.unitFraction))

			def getValue(left: Constant, right: Constant) = {
				if (left.isNumberConstant) Some(doCalculation(left, right))
				else
				/*left.getType match
  			{
					case DataType.undefined =>*/ Some(Expression.NullDOUBLE)
				/*case _ =>  throw new IllegalArgumentException( "Wrong Datatype "+left.getType+" for div operation ")
      } */

			}
		}
		,
		'^' -> new BinOperator('^', 3) {
			def getLongValue(left: Long, right: Long): Long = math.pow(left, right).longValue

			def getDoubleValue(left: Double, right: Double): Double = math.pow(left, right)

			def getCurrValue(left: Long, right: Long): Long = math.pow(left, right).longValue

			def getUnitValue(left: UnitNumber, right: UnitNumber) = new UnitNumber(math.pow(left.value, right.value), left.unitFraction.pot(right.value))

			def getValue(left: Constant, right: Constant) = {
				if (left.isNumberConstant) Some(doCalculation(left, right))
				else
					left.getType match {
						case DataType.undefined => Some(Expression.NullDOUBLE)
						case _ => throw new IllegalArgumentException("Wrong Datatype " + left.getType + " for pow operation ")
					}
			}
		},
		'=' -> new CompBinOperator('=') {
			def getValue(left: Constant, right: Constant) = {
				left.getType match {
					case DataType.DoubleTyp | DataType.CurrencyTyp | DataType.UnitNumberTyp => Some(new BoolConstant(left.getValue.toDouble == right.getValue.toDouble))
					case _ => Some(new BoolConstant(left.getValue == right.getValue))
				}

			}
		},
		'<' -> new CompBinOperator('<') {
			def getValue(left: Constant, right: Constant) = {
				left.getType match {
					case DataType.StringTyp => Some(new BoolConstant(left.getValue.toString < right.getValue.toString))
					case DataType.DoubleTyp | DataType.CurrencyTyp | DataType.UnitNumberTyp => Some(new BoolConstant(left.getValue.toDouble < right.getValue.toDouble))
					case _ => Some(new BoolConstant(left.getValue.toInt < right.getValue.toInt))
				}
			}
		},
		'>' -> new CompBinOperator('>') {
			def getValue(left: Constant, right: Constant) = {
				left.getType match {
					case DataType.StringTyp => Some(new BoolConstant(left.getValue.toString > right.getValue.toString))
					case DataType.DoubleTyp | DataType.CurrencyTyp | DataType.UnitNumberTyp => Some(new BoolConstant(left.getValue.toDouble > right.getValue.toDouble))
					case _ => Some(new BoolConstant(left.getValue.toInt > right.getValue.toInt))
				}
			}
		},
		'&' -> new CompBinOperator('&') {
			def getValue(left: Constant, right: Constant) = {
				Some(new BoolConstant(left.toBoolean && right.toBoolean))
			}
		},
		'|' -> new CompBinOperator('|') {
			def getValue(left: Constant, right: Constant) = {
				Some(new BoolConstant(left.toBoolean || right.toBoolean))
			}
		}
	)
	
	
}