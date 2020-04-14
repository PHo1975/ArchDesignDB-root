/**
  * Author: Peter Started:26.06.2010
  */
package definition.typ

/**
  * Enum of all possible data types for an expression and data field
  */


object DataType extends Enumeration {
  lazy val wrapMap: Map[DataType.Value, DTWrap] = values.toList.map(a => a -> DTWrap(a.toString, a)).toMap
  lazy val wrappedValues: Seq[DTWrap] = values.unsorted.map(a => DTWrap(a.toString, a)).toSeq
  val IntTyp = Value("Integer") //0
  val LongTyp = Value("Long") //1
  val DoubleTyp = Value("Double") //2
  val CurrencyTyp = Value("Currency") //3
  val StringTyp = Value("String") //4
  val BoolTyp = Value("Boolean") //5
  val DateTyp = Value("Date") //6
  val ObjectRefTyp = Value("ObjRef") //7
  val undefined = Value("UNDEFINED") //8
  val BinOp = Value("BinOp") //9
  val FieldRefTyp = Value("FieldRef") //10
  val FunctionCall = Value("FuncCall")
  //11
  val CollFunctionCall = Value("CollFuncCall")
  //12
  val VectorTyp = Value("Vector") //13
  val EnumTyp = Value("Enum") //14
  val ParentRefTyp = Value("ParentRef") //15
  val BlobTyp = Value("Blob") //16
  val PolygonTyp = Value("Polygon") //17
  val VariableTyp = Value("Variable") //18
  val UnitNumberTyp = Value("UnitNumber") //19
  val VectorBlockRef = Value("VectorBlockRef") //20
  val IntListTyp= Value("IntList")//21

  def isCompatible(one: Value, other: Value): Boolean = {
    if (one == undefined || other == undefined) true
    else if (one.id > other.id) isCompatible(other, one)
    else if (one.id == other.id) true
    else one match {
      case IntTyp => other match {
        case LongTyp => true
        case DoubleTyp => true
        case CurrencyTyp => true
        case ParentRefTyp => true
        case FieldRefTyp => true
        case UnitNumberTyp => true
        case _ => false
      }
      case LongTyp => other match {
        case DoubleTyp | CurrencyTyp => true
        case ParentRefTyp => true
        case FieldRefTyp => true
        case UnitNumberTyp => true
        case _ => false
      }
      case DoubleTyp => other match {
        case CurrencyTyp => true
        case ParentRefTyp => true
        case FieldRefTyp => true
        case UnitNumberTyp => true
        case _ => false
      }
      case CurrencyTyp => other match {
        case UnitNumberTyp => true
        case _ => false
      }

      case StringTyp => true
      case BoolTyp => other match {
        case IntTyp | LongTyp | BoolTyp => true
        case ParentRefTyp => true
        case FieldRefTyp => true
        case _ => false
      }
      case _ => false
    }
  }
}


case class DTWrap(name: String, typ: DataType.Value) {
  override def toString: String = name
}


