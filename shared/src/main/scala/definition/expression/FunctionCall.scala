/**
  * Author: Peter Started:07.08.2010
  */
package definition.expression

import java.io.{DataInput, DataOutput}

import definition.typ.DataType


/** An expression that manages a function call
  *
  */
case class FunctionCall(module: Option[String], name: String, params: List[Expression]) extends Expression {

  def getType: DataType.Value = DataType.FunctionCall

  def getValue: Constant = {
    val paramValues: List[Constant] = for (param <- params) yield param.getValue
    if (FunctionManager.get == null) {util.Log.e("No FunctionManager defined !"); EMPTY_EX}
    else FunctionManager.get.getFunctionValue(module, name, paramValues)
  }

  override def createCopy(): Expression = new FunctionCall(module, name, params.map(_.createCopy()))

  def getChildCount: Int = params.size

  def getChildNr(ix: Int): Expression = {params(ix)}

  def getTerm: String =
    (module match {
      case Some(mod) => mod + "."
      case _ => ""
    }) + name + "( " + params.map(_.getTerm).mkString("; ") + " ) "

  override def getReadableTerm: String = (module match {
    case Some(mod) => mod + "."
    case _ => ""
  }) + name + "( " + params.map(_.getReadableTerm).mkString("; ") + " ) "

  override def getReadableTerm(format:String): String = (module match {
    case Some(mod) => mod + "."
    case _ => ""
  }) + name + "( " + params.map(_.getReadableTerm(format)).mkString("; ") + " ) "


  def isConstant: Boolean = {false}

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.FunctionCall.id)
    file.writeBoolean(module.isDefined)
    if (module.isDefined) file.writeUTF(module.get)
    file.writeUTF(name)
    file.writeByte(params.size)
    for (p <- params)
      p.write(file)
  }

  override def getElementList[T <: Expression](whatType: DataType.Value, resultList: List[T]): List[T] = {
    var result = super.getElementList(whatType, resultList)
    for (p <- params)
      result = p.getElementList(whatType, result)
    result
  }

  override def replaceExpression(checker: Expression => Expression): Expression = {
    val newMe = checker(this)
    if (newMe == this) new FunctionCall(module, name, for (p <- params)
      yield p.replaceExpression(checker))
    else newMe
  }

  def encode: String = {
    "$F" + (module match {case Some(mod) => mod + "."; case _ => ""}) +
      name + "(" + params.map(_.encode).mkString(";") + ")"
  }

  override def containsString(st: String, checkNumbers: Boolean): Boolean = name.contains(st) || params.exists(_.containsString(st, checkNumbers))

}


object FunctionCall {
  def decode(text: String): (Expression, Int) = {
    val parStart = text.indexOf('(', 2)
    val front = text.substring(2, parStart).split('.')
    //println("decode func:"+front.mkString(","))+" "+front.size
    val module = if (front.length == 2) Some(front(0)) else None
    val name = if (front.length == 2) front(1) else front(0)
    var pos = parStart + 1
    var params = List[Expression]()
    if (text.charAt(pos) == ')') (new FunctionCall(module, name, List.empty), pos + 1)
    else {
      while (text.charAt(pos - 1) != ')') {
        val ex = Expression.decode(text.substring(pos, text.length))
        params = ex._1 :: params
        pos += ex._2 + 1
      }
      (new FunctionCall(module, name, params.reverse), pos)
    }
  }

  private[expression] def apply(file: DataInput): Expression = {
    val hasModule = file.readBoolean
    var moduleName: String = ""
    if (hasModule) moduleName = file.readUTF
    new FunctionCall(if (hasModule) Some(moduleName) else None, file.readUTF,
      (for (_ <- 0 until file.readByte) yield Expression.read(file)).toList)
  }

}