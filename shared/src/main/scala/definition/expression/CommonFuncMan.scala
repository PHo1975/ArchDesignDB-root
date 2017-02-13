package definition.expression

import definition.typ.DataType
import definition.data.InstanceData
import definition.data.Reference
import scala.util.control.NonFatal

trait VariableResolver {   
  def moduleName:String
  def variableExists(varName:String):Boolean
  def getVariableValue(varName:String):Constant
  def listVariables(forData:InstanceData):Iterable[String]
}

class CommonFuncMan extends FunctionManager {  
  var variableResolvers:Map[String,VariableResolver]=Map.empty
	
  registerVariableResolver(new MathVariableResolver)
  
	protected def checkParameters(param:List[Constant],paramDesc:List[ParDes]):String =   {
		//System.out.println("check params "+param+" "+paramDesc)
    if(paramDesc.isEmpty) null // no check 
    else if(paramDesc.size!=param.size) return "Wrong number of parameters "+param.size+", expected:"+paramDesc.size
  	 for(i <- 0 until param.size)
  		 if((paramDesc(i).typ!=DataType.undefined)&&(! DataType.isCompatible(param(i).getType ,paramDesc(i).typ))) 
  			 return "Wrong " + i + ". function parameter type " + param(i).getType + ", expected Type:" + paramDesc(i).typ
  	 null
  }
	
	val funcList= Map[String,FEntry] (
			"sin" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.sin(x.head.toDouble))
			),
			"cos" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.cos(x.head.toDouble))
			),
			"tan" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.tan(x.head.toDouble))
			),
			"atan" -> FEntry(List(ParDes(DataType.DoubleTyp), ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.atan2(x.head.toDouble, x(1).toDouble))
			),
			"sqrt" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.sqrt(x.head.toDouble))
			),
			"log10" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.log10(x.head.toDouble))
			),
			"log" -> FEntry(List(ParDes(DataType.DoubleTyp)), x =>
				new DoubleConstant(math.log(x.head.toDouble))
			),
			"max" -> FEntry(List(ParDes(DataType.DoubleTyp), ParDes(DataType.DoubleTyp)), x => {
				val v1 = x.head.toDouble
				val v2 = x(1).toDouble
				//System.out.println("call max "+x)
				new DoubleConstant(if (v1 < v2) v2 else v1)
			}
			),
			"if" -> FEntry(List(ParDes(DataType.undefined), ParDes(DataType.undefined), ParDes(DataType.undefined)), x => {
				if (x.head.toBoolean) x(1).getValue()
				else x(2).getValue()
			}),
			"v" -> FEntry(List(ParDes(DataType.undefined), ParDes(DataType.undefined), ParDes(DataType.undefined)), x => {
				new VectorConstant(x.head.toDouble, x(1).toDouble, x(2).toDouble)
			}),
			"runden" -> FEntry(List(ParDes(DataType.DoubleTyp), ParDes(DataType.IntTyp)), x => {
				val num = x.head.toDouble
				val digits = x(1).toInt
				val exp = math.pow(10, digits)
				val result = math.round(num * exp) / exp
				x.head match {
					case e: UnitNumber => new UnitNumber(result, e.unitFraction)
					case _ => new DoubleConstant(result)
				}

			}),
			"formatcurrency" -> FEntry(List(ParDes(DataType.undefined)), x => {
				new StringConstant("%,.2f €".format(x.head.toDouble))
			}),
			"numberformat" -> FEntry(List(ParDes(DataType.undefined), ParDes(DataType.StringTyp)), x => {
				val num = x.head.toDouble
				val form = x(1).toString
				//val exp=math.pow(10,digits)
				try
					new StringConstant(form.format(num) + (x.head match {
						case ut: UnitNumber => " " + ut.unitFraction.toString
						case _ => ""
					}))
				catch {
					case NonFatal(e) => new StringConstant(e.getMessage)
				}
			}),
			"concat" -> FEntry(Nil, x => {
				if (x.size < 2) new StringConstant("2 Parameter erforderlich " + x.size)
				new StringConstant(x.map(_.toString).mkString)
			}),
			"lookupover" -> FEntry(Nil, x => {
				val num = x.head.toDouble
				if (x.size < 3 || x.size % 2 == 0) new StringConstant("Falsche Anzahl Parameter")
				else {
					x.drop(1).grouped(2).find { case List(key, _) => key.toDouble >= num } match {
						case Some(List(key, value)) => value.convertTo(DataType.DoubleTyp)
						case _ => new StringConstant("Fehler")
					}
				}
			}),
			"double" -> FEntry(List(ParDes(DataType.undefined)), x =>
				new DoubleConstant(x.head.toDouble)
			)
	)	
	
	
	def getFunctionValue(module:Option[String],funcName:String,paramValues:List[Constant]) = {
		val uname=funcName.toLowerCase
		//System.out.println("call funcman :"+funcName+" "+paramValues)
		if(funcList.contains(uname))
		{
			val entry =funcList(uname)
			if(entry.params.nonEmpty) {
				val error=checkParameters(paramValues,entry.params)
				if(error!=null)throw new IllegalArgumentException(error+" in function "+funcName+"\n params:"+
					paramValues.mkString(",")+"\n types:" +paramValues.map(_.getClass.toString).mkString(","))
			}	
			entry.func(paramValues)			
		}		  
		else throw new IllegalArgumentException("Function "+uname+" not found")		
	}
	
  protected val sumFunc= new SingleCollFunction("doubleSum"){
	    	def childAdded(oldResult:Constant,newValue:Constant):Constant = {
	    	  //println("SumFunc childAdded oldResult:"+oldResult+" newValue:"+newValue+" res:"+BinOperator.plusOp.getValue(oldResult,newValue).get)
	    	  BinOperator.plusOp.getUnitSafeValue(oldResult,newValue,false).get	    	  	
	    	}	
	      def childChanged(oldResult:Constant,oldValue:Constant,newValue:Constant):Option[Constant]	= 
	       BinOperator.plusOp.getUnitSafeValue(if(oldValue.isNullConstant)oldResult else 
	         BinOperator.minusOp.getUnitSafeValue(oldResult,oldValue,true).get,newValue,false)
	      
	       
		    def childRemoved(oldResult:Constant,oldValue:Constant):Option[Constant] =   BinOperator.minusOp.getUnitSafeValue(oldResult,oldValue,true)		    	
		    	   
	    	def emptyValue:Constant = new DoubleConstant(0)	
	    }
	
  protected val currencySumFunc= new SingleCollFunction("currencySum"){
	    	def childAdded(oldResult:Constant,newValue:Constant):Constant = {	    	  
	    	  BinOperator.plusOp.getUnitSafeValue(oldResult.toCurrency,newValue.toCurrency,false).get	    	  	
	    	}	
	      def childChanged(oldResult:Constant,oldValue:Constant,newValue:Constant):Option[Constant]	= 
	       BinOperator.plusOp.getUnitSafeValue(if(oldValue.isNullConstant)oldResult.toCurrency else 
	         BinOperator.minusOp.getUnitSafeValue(oldResult.toCurrency,oldValue.toCurrency,true).get,newValue.toCurrency,false)
	      
	       
		    def childRemoved(oldResult:Constant,oldValue:Constant):Option[Constant] =   BinOperator.minusOp.getUnitSafeValue(oldResult,oldValue,true)		    	
		    	   
	    	def emptyValue:Constant = new DoubleConstant(0)	
	    }
  
	val collFuncList= Map[String,CollectingFunction] (
	    "doubleSum" -> sumFunc, "summe" -> sumFunc	, "textSum" -> new ListCollFunction("textSum") {
			def listChanged(newList: List[(Reference, Constant)]): Constant = {
				StringConstant(newList.map({ case (ref, const) => const.toString }).mkString(", "))
			}

			def emptyValue: Constant = EMPTY_EX
		}, "currencySum" -> currencySumFunc
	)
	
	def getVariableValue(module:String,varName:String):Constant = {
	  if(variableResolvers.contains(module)) {
	    variableResolvers(module).getVariableValue(varName)
	  }else {
      util.Log.e("Cant find Variable Module:"+module+" var:"+varName+" ",Thread.currentThread().getStackTrace)
	    new StringConstant("Unbekanntes VariablenModul '"+module+"' "+varName)
	  }
	}
	
	def registerVariableResolver(newRes: VariableResolver)=
    variableResolvers=variableResolvers++Map(newRes.moduleName->newRes)	  	 
}


// *********************************** HELPER CLASSES ********************************************



case class FEntry(params:List[ParDes],func:(List[Constant])=>Constant)

// parameter description
case class ParDes(typ:DataType.Value,name:String="",help:String=""  )

