/**
  * Author: Peter Started:18.07.2010
  */
package definition.expression

import java.io.DataOutput

import definition.typ.DataType
//import java.util.Date
import definition.data.{EMPTY_REFERENCE, Reference}

/** base type of all Constant classes
  *
  */
trait Constant extends Expression {

  def getChildCount: Int = 0

  def getChildNr(ix: Int): Expression = {util.Log.e("Constants don't have any children, access to child nr:" + ix); this}

  def isConstant: Boolean = {true}

  def getValue: Constant = this

  def toInt: Int

  def toLong: Long

  def toDouble: Double

  def toFloat: Float = toDouble.toFloat

  def toCurrency = new CurrencyConstant(math.round(toDouble * 100))

  def toBoolean: Boolean

  def toVector: VectorConstant = NULLVECTOR

  def toUnitNumber = new UnitNumber(toDouble, UnitNumber.emptyFraction)

  def convertTo(toType: DataType.Value): Constant = Constant.createConversion(this, toType)

  def getNative: Any

  def toDate = DateConstant(toLong)

  def isNumberConstant = false

  def toObjectReference: Reference = EMPTY_REFERENCE

  def toPolygon: Polygon = NULLPOLYGON
}


object Constant {
  /** creates a converted Contstant in the given type
    *
    * @param value  Original value
    * @param toType in what type should this value be converted
    * @return A new Constant with the value, converted to another type
    */
  def createConversion(value: Constant, toType: DataType.Value): Constant =
    if (value.getType == toType || toType == DataType.undefined) value
    else toType match {
      case DataType.IntTyp | DataType.EnumTyp => IntConstant(value.toInt)
      case DataType.LongTyp => LongConstant(value.toLong)
      case DataType.DoubleTyp => DoubleConstant(value.toDouble)
      case DataType.StringTyp => StringConstant(value.toString)
      case DataType.BoolTyp => BoolConstant(value.toBoolean)
      case DataType.CurrencyTyp => value.toCurrency
      case DataType.UnitNumberTyp => value.toUnitNumber
      case DataType.VectorTyp => val nv = value.toVector; new VectorConstant(nv.x, nv.y, nv.z)
      case DataType.DateTyp => DateConstant(value.toLong)
      case DataType.BlobTyp => Expression.NullBLOB
      case _ => throw new IllegalArgumentException("Conversion of " + value + "  to type " + toType + " is not supported yet")
    }

  def getNativeNull(typ: DataType.Value): Any = typ match {
    case DataType.IntTyp | DataType.EnumTyp => 0
    case DataType.LongTyp => 0L
    case DataType.DoubleTyp => 0d
    case DataType.StringTyp => ""
    case DataType.BoolTyp => false
    case DataType.VectorTyp => NULLVECTOR
    case DataType.CurrencyTyp => Expression.NullCURRENCY
    case DataType.UnitNumberTyp => Expression.NULLUNITNUMBER
    case DataType.BlobTyp => Array[Byte]()
    case DataType.DateTyp => DateConstant.nativeNull
    case DataType.ObjectRefTyp => new ObjectReference(0, 0)
    case DataType.PolygonTyp => NULLPOLYGON
    case _ => ""
  }

}


object EMPTY_EX extends Constant with Serializable {
  def getType: DataType.Value = DataType.undefined

  //def createCopy():Expression= this

  def toInt: Int = 0

  def toLong: Long = 0

  def toDouble: Double = 0

  def toBoolean: Boolean = false

  override def toCurrency:CurrencyConstant = Expression.NullCURRENCY

  def getTerm: String = ""

  def write(file: DataOutput): Unit = file.writeByte(DataType.undefined.id)

  def getNative = ""

  override def toString = ""

  override def isNullConstant = true

  override def toDate: DateConstant = DateConstant.NULL_DATE

  def encode = "$N"
}


object TRUE extends BoolConstant(true)


object FALSE extends BoolConstant(false)