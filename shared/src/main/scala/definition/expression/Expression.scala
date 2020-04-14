/**
  * Author: Peter Started:18.07.2010
  */
package definition.expression

import java.io.{DataInput, DataOutput}

import definition.typ.DataType
import util.Log

import scala.util.control.NonFatal

/** base type for all expression classes
  *
  */
trait Expression extends ParserResult {
  def getType: DataType.Value

  def getValue: Constant

  def getChildCount: Int

  def getChildNr(ix: Int): Expression

  def getTerm: String

  def getReadableTerm: String = getTerm

  def getReadableTerm(format:String):String=getReadableTerm

  def isConstant: Boolean

  def write(file: DataOutput): Unit

  def isNullConstant: Boolean = false

  def containsString(st: String, checkNumbers: Boolean): Boolean = false

  /** is called when generating an instance
    * will be overridden by generatorConstants like $Now and $Today
    *
    * @return the actual state of that expression during generation time
    */
  def generate: Expression = createCopy()

  def createCopy(): Expression = this

  /** returns all Elements of the given type from this expression
    *
    * @param whatType   Type-Id from definition.typ.DataType
    * @param resultList a List of elements of the given type. The elements of this expression will be added to that list.
    * @return a new list containing all elements of resultList and the elements of this list fitting to the given type
    */
  def getElementList[T <: Expression](whatType: DataType.Value, resultList: List[T]): List[T] =
    if (this.getType == whatType) this.asInstanceOf[T] :: resultList
    else resultList


  /** replaces certain elements of this term
    *
    * @param checker a function that is given a certain part of this term and it gives a replacement
    * @return the new term with all replacements
    */
  def replaceExpression(checker: Expression => Expression): Expression =
    checker(this)

  def foreach(f: Expression => Unit): Unit = {
    f(this)
    var i = 0
    while (i < getChildCount) {
      getChildNr(i).foreach(f)
      i += 1
    }
  }

  def exists(pred: Expression => Boolean): Boolean =
    if (pred(this)) true
    else {
      var i = 0
      while (i < getChildCount) {
        if (pred(getChildNr(i))) return true
        i += 1
      }
      false
    }


  /** encodes the expression in a string representation
    *
    */
  def encode: String

  def withException: Expression = this
}


trait NullConstant extends Expression {
  override def isNullConstant = true

  override def write(file: DataOutput): Unit =
    file.writeByte(DataType.undefined.id)

}


object Expression {
  lazy val NullINT = new IntConstant(0) with NullConstant
  lazy val NullLONG = new LongConstant(0) with NullConstant
  lazy val NullDOUBLE = new DoubleConstant(0d) with NullConstant
  lazy val NullSTRING = new StringConstant("") with NullConstant
  lazy val NullBOOL = new BoolConstant(false) with NullConstant
  lazy val NullVECTOR = new VectorConstant(0, 0, 0) with NullConstant
  lazy val NullCURRENCY = new CurrencyConstant(0) with NullConstant
  lazy val NullBLOB = new BlobConstant(new Array[Byte](0)) with NullConstant
  lazy val NullDATE = DateConstant()
  lazy val NULLOBJREF = new ObjectReference(0, 0)
  lazy val NULLUNITNUMBER = new UnitNumber(0, UnitNumber.emptyFraction)
  lazy val NULLINTLIST=new IntList(Array.empty)

  def read(file: DataInput): Expression = {
    val code = file.readByte
    if (code < 0 || code > DataType.maxId) throw new IllegalArgumentException("Reading Expression: cant find Datatype for code: " + code)
    DataType(code) match {
      case DataType.undefined => EMPTY_EX
      case DataType.IntTyp => IntConstant(file.readInt)
      case DataType.LongTyp => LongConstant(file.readLong)
      case DataType.DoubleTyp => DoubleConstant(file.readDouble)
      case DataType.StringTyp => StringConstant(file.readUTF)
      case DataType.BinOp => BinaryOperation(file)
      case DataType.FieldRefTyp => FieldReference(file)
      case DataType.FunctionCall => FunctionCall(file)
      case DataType.CollFunctionCall => CollectingFuncCall(file)
      case DataType.BoolTyp => BoolConstant(file.readBoolean)
      case DataType.VectorTyp => new VectorConstant(file.readDouble, file.readDouble, file.readDouble)
      case DataType.CurrencyTyp => new CurrencyConstant(file.readLong)
      case DataType.ParentRefTyp => ParentFieldRef(file)
      case DataType.BlobTyp => BlobConstant(file)
      case DataType.DateTyp => DateConstant(file)
      case DataType.ObjectRefTyp => ObjectReference(file)
      case DataType.PolygonTyp => Polygon(file)
      case DataType.VariableTyp => Variable(file)
      case DataType.UnitNumberTyp => UnitNumber(file)
      case DataType.IntListTyp=> IntList(file)
      case o => throw new IllegalArgumentException("Unknown Datatype " + o + " when read expression")
    }
  }

  def readConstant(file: DataInput): Constant =
    DataType(file.readByte()) match {
      case DataType.undefined => EMPTY_EX
      case DataType.IntTyp => IntConstant(file.readInt)
      case DataType.LongTyp => LongConstant(file.readLong)
      case DataType.DoubleTyp => DoubleConstant(file.readDouble)
      case DataType.StringTyp => StringConstant(file.readUTF)
      case DataType.BoolTyp => BoolConstant(file.readBoolean)
      case DataType.VectorTyp => new VectorConstant(file.readDouble, file.readDouble, file.readDouble)
      case DataType.CurrencyTyp => new CurrencyConstant(file.readLong)
      case DataType.BlobTyp => BlobConstant(file)
      case DataType.DateTyp => DateConstant(file)
      case DataType.ObjectRefTyp => ObjectReference(file)
      case DataType.PolygonTyp => Polygon(file)
      case DataType.UnitNumberTyp => UnitNumber(file)
      case DataType.IntListTyp=> IntList(file)
      case o => throw new IllegalArgumentException("Unknown Datatype " + o + " when read Constant")
    }


  def generateNullConstant(typ: DataType.Value): Constant =
    typ match {
      case DataType.IntTyp => NullINT
      case DataType.LongTyp => NullLONG
      case DataType.DoubleTyp => NullBOOL
      case DataType.StringTyp => NullSTRING
      case DataType.BoolTyp => NullBOOL
      case DataType.VectorTyp => NullVECTOR
      case DataType.CurrencyTyp => NullCURRENCY
      case DataType.EnumTyp => NullINT
      case DataType.DateTyp => NullDATE
      case DataType.BlobTyp => NullBLOB
      case DataType.ObjectRefTyp => NULLOBJREF
      case DataType.PolygonTyp => NULLPOLYGON
      case DataType.UnitNumberTyp => NULLUNITNUMBER
      case DataType.IntListTyp => NULLINTLIST
      case _ => EMPTY_EX
    }


  def decode(text: String): (Expression, Int) =
    if (text.length == 0) (EMPTY_EX, 0)
    else text.charAt(0) match {
        case '$' => text.charAt(1) match {
          case 'A' | 'a' => Polygon.decode(text)
          case 'B' | 'b' => BinaryOperation.decode(text)
          case 'C' | 'c' => CurrencyConstant.decode(text)
          case 'D' | 'd' => TodayConstant.decode(text)
          case 'E' | 'e' => UnitNumber.decode(text)
          case 'F' | 'f' => FunctionCall.decode(text)
          case 'G' | 'g' => DoubleConstant.decode(text)
          case 'I' | 'i' =>
            val end = CurrencyConstant.findEnd(text, 2)
            (IntConstant(text.substring(2, end).toInt), end)
          case 'L' | 'l' =>
            val end = CurrencyConstant.findEnd(text, 2)
            (LongConstant(text.substring(2, end).toLong), end)
          case 'N' | 'n' => (EMPTY_EX, 2)
          case 'O' | 'o' => ObjectReference.decode(text)
          case 'P' | 'p' => ParentFieldRef.decode(text)
          case 'R' | 'r' => FieldReference.decode(text)
          case 'X' | 'x' => (new BlobConstant(Array[Byte](0)), 2)
          case 'S' | 's' => CollectingFuncCall.decode(text)
          case 'T' | 't' => (BoolConstant(text.charAt(2) == '1'), 3)
          case 'U' | 'u' => DateConstant.decode(text)
          case 'V' | 'v' => VectorConstant.decode(text)
          case 'Y' | 'y' => Variable.decode(text)
          case 'Z' | 'z' => IntList.decode(text)
          case other => throw new IllegalArgumentException("Decode : unknown tag " + other + " " + text)
        }
        case '"' =>
          val pos = text.indexOf('"', 1)
          if (pos < 0) throw new IllegalArgumentException("decode stringConstant " + text + " unclosed String")
          (StringConstant(text.substring(1, pos).replace('«', '"')), pos + 1)
        case _ => throw new IllegalArgumentException("cant decode :'" + text + "', unknown tag " + text.length)
      }


}


object Decode {
  def unapply(st: String): Option[Expression] = try {
    Some(Expression.decode(st)._1)
  }
  catch {
    case NonFatal(e) => Log.e("Error decoding ", e); None
  }
}

