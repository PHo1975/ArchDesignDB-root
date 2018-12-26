/**
  * Author: Peter Started:23.07.2010
  */
package definition.expression

import definition.typ.DataType

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

trait ParserResult {
  def withException: Expression
}


case class ParserError(_message: String, offset: Int) extends ParserResult {
  lazy val message: String = { // translate
    if (_message == null) "null" else if (_message.contains("""string matching regex `\z""")) {
      if (_message.charAt(41) == ')') "öffnende Klammer fehlt"
      else "Unerwartetes Zeichen '" + _message.charAt(41) + "'"
    } else if (_message.contains("""`)' expected""")) "Schliessende Klammer fehlt"
    else if (_message.contains("""`(' expected""")) "öffnende Klammer fehlt, oder doppelte Rechenzeichen"
    else if (_message.contains("""`_' expected but end""")) "Nur Eingabe von Zahl möglich"
    else _message
  }

  def withException: Expression = {
    throw new IllegalArgumentException(message + " Offset:" + offset)
  }
}


/**
  * parses a String into an expression
  *
  */
class StringParser extends JavaTokenParsers {
  override val skipWhitespace = false
  val UnitElemMatcher: Regex ="""([a-zA-ZäöüÄÖÜ\u00A3\u20AC]+)(\d*)""".r

  def intNumber: Parser[String] =
    """-?\d+""".r

  def doubleNumber: Parser[String] =
    """-?((\d+)[\.,]\d*|\d*[\.,]\d+)""".r

  def date: Parser[String] = """[0-3]?[0-9]\.[0-1]?[0-9]\.[0-9]{2,4}""".r

  def groupedNumber: Parser[String] = """-?^[0-9]{1,3}(\.[0-9]{3})+(\,[0-9]+)?$""".r

  def numberParam: Parser[String] = """\s?\d+\s?""".r

  def currencySymbols: Parser[String] = """[\u00A3\u20AC]""".r

  def unitSymbol: Parser[String] = """[a-zA-ZäöüÄÖÜ\u00A3\u20AC]+""".r

  def unitElem: Parser[UnitElem] = UnitElemMatcher ^^ {
    case UnitElemMatcher(s, n) => new UnitElem(s, if (n.isEmpty) 1 else n.toInt.toByte)
    case y => new UnitElem(y, 1)
  }

  def unitList: Parser[List[UnitElem]] =
    ((unitElem ~ rep("·" ~> unitElem)) ^^ { case ex ~ list => ex :: list }) |
      (unitElem ^^ (List(_)))

  def someNumber: Parser[String] = """-?(\d+[\.,]\d*|-?\d*[\.,]\d+|\d+)""".r

  def unitNumber: Parser[Expression] = (someNumber ~ unitList ~ "/" ~ unitList) ^^ { case num ~ nom ~ _ ~ den => new UnitNumber(makeDouble(num), UnitFraction(UnitNumber.setFactory ++ nom, UnitNumber.setFactory ++ den)) } |
    (someNumber ~ unitList) ^^ { case num ~ list => new UnitNumber(makeDouble(num), UnitFraction(UnitNumber.setFactory ++ list, UnitNumber.emptySet)) }

  //(someNumber~  ("/".r~>unitList))^^{case num~den=>new UnitNumber(makeDouble(num),new UnitFraction(UnitNumber.emptySet,UnitNumber.setFactory++den))}

  def nullParser: Parser[Expression] = """null""".r ^^ (_ => EMPTY_EX)

  def variableNamePart: Parser[String] = """[a-zA-Z0-9]+""".r

  def stringVariable: Parser[String] = """["][^"]*["]""".r

  def todayConstant: Parser[String] = TodayConstant.todayString.r

  def comp: Parser[Expression] = (expr ~ rep("=" ~ expr | "<" ~ expr | ">" ~ expr | "&" ~ expr | "|" ~ expr)) ^^ {
    case first ~ list =>
      list.foldLeft(first)((res, value) => BinaryOperation(res, BinOperator.getOp(value._1.charAt(0)), value._2))
  }

  def nonWhiteComp: Parser[Expression] = whiteSpace ~> comp <~ whiteSpace ||| whiteSpace ~> comp ||| comp <~ whiteSpace ||| comp

  def expr: Parser[Expression] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case first ~ list =>
      list.foldLeft(first)((res, value) => BinaryOperation(res, BinOperator.getOp(value._1.charAt(0)), value._2))
  }

  def term: Parser[Expression] = (factor ~ rep("*" ~ factor | "·" ~ factor | "/" ~ factor | "%" ~ factor)) ^^ {
    case first ~ list =>
      list.foldLeft(first)((res, value) => BinaryOperation(res, BinOperator.getOp(value._1.charAt(0) match {
        case '*' => '*'
        case '·' => '*'
        case '%' => '%'
        case _ => '/'
      }), value._2))
  }

  //def nonWhiteTerm= term|whiteSpace~>term | term<~whiteSpace

  //def nonWhiteFactor= factor| whiteSpace ~>factor  | factor <~whiteSpace //| whiteSpace ~>factor <~ whiteSpace 

  def factor: Parser[Expression] = (nonWhiteElem ~ rep("^" ~ nonWhiteElem)) ^^ {
    case first ~ list =>
      list.foldLeft(first)((res, value) => BinaryOperation(res, BinOperator.getOp('^'), value._2))
  }

  def fieldRef: Parser[Expression] =
    ("[#][Tt]".r ~> intNumber ~ ("[iI]".r ~> intNumber) ~ ("[fF]".r ~> intNumber)) ^^ {
      case typ ~ inst ~ field => new FieldReference(Some(typ.toInt), Some(inst.toInt), field.toByte)
    } |
      ("[#][iI]".r ~> intNumber ~ ("[fF]".r ~> intNumber)) ^^ {
        case inst ~ field => new FieldReference(None, Some(inst.toInt), field.toByte)
      } |
      ("[#][fF]".r ~> intNumber) ^^ (field => new FieldReference(None, None, field.toByte)) | failure("falsche FeldReferenz")

  def parentFieldRef: Parser[Expression] = ("[#][Pp]".r ~> intNumber ~ ("[Ff]".r ~> intNumber)) ^^ {
    case ownerIx ~ fieldNr => new ParentFieldRef(ownerIx.toByte, fieldNr.toByte)
  }

  def elem: Parser[Expression] = date ^^ {
    st =>
      val parts = st.split('.')
      val year = parts(2).toInt
      DateConstant(parts(0).toInt, parts(1).toInt, if (year < 100) 2000 + year else year)
  } | (currValue ||| unitNumber) |
    groupedNumber ^^ { y =>
      val doubleVal = y.replace(".", "").replace(',', '.').toDouble
      if (StringParser.isIntValue(doubleVal)) IntConstant(doubleVal.toInt) else DoubleConstant(doubleVal)
    } | nullParser |
    doubleNumber ^^ { y => DoubleConstant(y.replace(',', '.').toDouble) } |
    intNumber ^^ { x => IntConstant(x.toInt) } | trueVal | falseVal |
    stringVariable ^^ { s => StringConstant(s.substring(1, s.length - 1)) } |
    fieldRef | function | variable | collFunction | parentFieldRef | vector | todayConstant ^^ { _ => TodayConstant } |
    ("(" ~> nonWhiteComp <~ ")") | failure("Zahl oder Wert fehlt")

  def nonWhiteElem: Parser[Expression] = whiteSpace ~> elem <~ whiteSpace ||| whiteSpace ~> elem ||| elem <~ whiteSpace ||| elem

  def paramList: Parser[List[Expression]] =
    ((nonWhiteComp ~ rep(";" ~> nonWhiteComp)) ^^ { case ex ~ list => ex :: list }) |
      (nonWhiteComp ^^ (List(_)))

  def currValue: Parser[Expression] =
    (someNumber ~ currencySymbols) ^^ { case a ~ _ => CurrencyConstant(math.round(a.replace(',', '.').toDouble * 100L)) }

  def function: Parser[Expression] =
    (ident ~ ("(" ~> paramList <~ ")")) ^^ { case name ~ list => FunctionCall(None, name, list) }

  def trueVal: Parser[Expression] = "true|TRUE".r ^^ (_ => BoolConstant(true))

  def falseVal: Parser[Expression] = "false|FALSE".r ^^ (_ => BoolConstant(false))

  def variable: Parser[Expression] = ((variableNamePart <~ "_") ~ variableNamePart) ^^ { case module ~ name => Variable(module, name) }

  def collFunction: Parser[Expression] = (("#" ~> ident <~ "(") ~ (numberParam <~ ";") ~ (numberParam <~ ";") ~ (numberParam <~ ")")) ^^ {
    case name ~ propField ~ childType ~ childField =>
      CollectingFuncCall(name, propField.trim.toByte, childType.trim.toInt, childField.trim.toByte)
  }

  def vector: Parser[Expression] =
    (("""[Vv][\[]""".r ~> doubleNumber <~ ";") ~ (doubleNumber <~ ";") ~ (doubleNumber <~ "]")) ^^ {
      case x ~ y ~ z => new VectorConstant(x.toDouble, y.toDouble, z.toDouble)
    }

  def makeDouble(st: String): Double = st.replace(',', '.').toDouble
}


object StringParser extends StringParser {
  def isIntValue(double: Double): Boolean = double == math.floor(double)

  def parse(text: String, expectedType: DataType.Value = DataType.undefined): ParserResult = {
    if (text.length == 0) Expression.generateNullConstant(expectedType)
    else {
      val result = parseAll(nonWhiteComp, text)
      result match {
        case Success(x, _) => x
        case NoSuccess(err, next) =>
          if (expectedType == DataType.StringTyp) StringConstant(text)
          else ParserError(if (err == null) "Null" else err, next.offset)
      }
    }
  }
}