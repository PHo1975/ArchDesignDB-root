package definition.expression

import java.io.{DataInput, DataOutput}

import definition.typ.DataType

@SerialVersionUID(14237L) case class Variable(module: String, name: String) extends Expression {

  def getType: DataType.Value = DataType.VariableTyp

  def getValue: Constant = if (FunctionManager.get == null) EMPTY_EX else FunctionManager.get.getVariableValue(module, name)

  def getChildCount: Int = 0

  def getChildNr(ix: Int): Expression = null

  def isConstant: Boolean = false

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.VariableTyp.id)
    file.writeUTF(module)
    file.writeUTF(name)
  }

  def encode: String = "$Y" + getTerm + ";"

  def getTerm: String = if (module.length == 0) name else module + "_" + name

  override def getReadableTerm: String = getValue.getReadableTerm

  override def containsString(st: String, checkNumbers: Boolean): Boolean = name.contains(st)

}


object Variable {
  def decode(text: String): (Expression, Int) = {
    val pos = text.indexOf(';', 5)
    val sub = text.substring(2, pos).split('_')
    if (sub.length != 2) throw new IllegalArgumentException("Cant decode Variable '" + text + "'")
    (Variable(sub(0), sub(1)), pos + 1)
  }

  private[expression] def apply(file: DataInput): Expression = Variable(file.readUTF, file.readUTF)
}