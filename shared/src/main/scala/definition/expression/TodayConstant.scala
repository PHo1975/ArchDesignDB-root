package definition.expression

import java.io.DataOutput
import definition.typ.DataType

object TodayConstant extends Expression {
  val todayString = "@heute"

  def getType = DataType.DateTyp

  def getValue: Constant = generate.getValue

  /** is called when generating an instance
    * will be overridden by generatorConstants like $Now and $Today
    *
    * @return the actual state of that expression during generation time
    */
  override def generate: Expression = DateConstant()

  override def createCopy(): Expression = this

  def getChildCount: Int = 0

  def getChildNr(ix: Int): Expression = EMPTY_EX

  def getTerm: String = todayString

  def isConstant: Boolean = true

  def write(file: DataOutput): Unit = {
  }

  def encode: String = "$D"

  def decode(text: String): (Expression, Int) = {
    (this, 2)
  }

}