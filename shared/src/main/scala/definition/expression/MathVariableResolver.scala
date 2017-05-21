package definition.expression

import definition.data.InstanceData

abstract class FixedVariableResolver extends VariableResolver {
  def varMap: Map[String, Constant]

  def variableExists(varName: String): Boolean = varMap.contains(varName.toLowerCase)

  def getVariableValue(varName: String): Constant =
    if (varMap.contains(varName.toLowerCase)) varMap(varName.toLowerCase)
    else {
      util.Log.e("Unknown Variable:" + varName + " in Module" + moduleName)
      StringConstant("Unbekannte Variable " + varName)
    }

  def listVariables(forData: InstanceData): Iterable[String] = varMap.keys
}


class MathVariableResolver extends FixedVariableResolver {
  val pi = new DoubleConstant(math.Pi)
  val varMap: Map[String, Constant] = Map[String, Constant](("pi", pi))

  def moduleName = "math"
}