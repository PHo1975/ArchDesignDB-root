/**
  * Author: Peter Started:07.08.2010
  */
package definition.expression

/** abstract interface for a function manager
  *
  */
trait FunctionManager {

  def getFunctionValue(module: Option[String], funcName: String, paramValues: List[Expression]): Constant

  def getVariableValue(module: String, varName: String): Constant

  //def getCollFunctionValue(funcName:String,propertyField:Byte,childType:Int,childField:Byte):Constant
  def registerVariableResolver(res: VariableResolver)

  def collFuncList: Map[String, CollectingFunction]
}


object FunctionManager {
  private var theManager: FunctionManager = _

  def get: FunctionManager = theManager

  def setManager(newMan: FunctionManager): Unit = {theManager = newMan}
}

