/**
 * Author: Peter Started:27.09.2010
 */
package definition.comm

import java.io._

import definition.data._

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.parsing.combinator._


case class UserInfo(id:Int,name:String,shortName:String) {  
}

/** reads and stores user settings
 * 
 */

abstract class PropertyValue{
	def name:String
	def writeString:String
	def value:Any
}

case class IntValue(override val name:String,override val value:Int) extends PropertyValue {
	def writeString= "{"+UserSetting.quoteString(name)+":"+value+"}"
}

case class StringValue(override val name:String,override val value:String) extends PropertyValue {
	def writeString="{"+UserSetting.quoteString(name)+":"+UserSetting.quoteString(value)+"}"
}

case class ListValue[T](override val name:String,override val value:collection.Seq[T]) extends PropertyValue {
	def writeString="{"+UserSetting.quoteString(name)+": ["+value.map {
    case i: Int => i.toString
    case s: String => UserSetting.quoteString(s)
    case r: Reference => r.sToString()
    case  (e1,e2) => "(" + e1 + ":" + e2 + ")"
    case g: PropertyGroup => g.writeString
  }.mkString(",")+"] }"
}

case class PropertyGroup(name:String, properties:collection.mutable.Map[String,PropertyValue]) {
	def writeString="{"+UserSetting.quoteString(name)+":"+properties.valuesIterator.map {
		_.writeString	}.mkString(" ")+ "}"
  def containsProperty(pName:String)= properties.contains(pName)
	def addProperty(p:PropertyValue)= properties(p.name)=p
	def getProperty(pName:String)= properties(pName)
	def getListProperty[T](pName:String):Seq[T]=properties(pName).asInstanceOf[ListValue[T]].value.asInstanceOf[Seq[T]]
  def getIntProperty(pName:String,defaultValue:Int=0):Int= if(properties.contains(pName)) properties(pName).asInstanceOf[IntValue].value else defaultValue
  def getStringProperty(pName:String,defaultValue:String=""):String=if (properties.contains(pName)) properties(pName).asInstanceOf[StringValue].value else defaultValue
}



// *****************************************************************************************************

class UserSetting {
  import UserSetting._
	private var groupList=mutable.HashMap[String,PropertyGroup]()
  
	var userList:Seq[UserInfo]=Nil
	
	var rootRef:Reference=EMPTY_REFERENCE
	var basicFolders:collection.immutable.Map[String,Reference]=collection.Map.empty
	
	def readFromStream(in:DataInput,length:Int,atClient:Boolean)= {
		val buffer=new Array[Byte](length)
		if(length>0) in.readFully(buffer,0,length)
		val s= new String(buffer,"UTF-8")
		parse(s)
    if(atClient) {
      rootRef = Reference(in)
      basicFolders = (for (i <- 0 until in.readInt) yield (in.readUTF, Reference(in))).toMap
      userList = for (i <- 0 until in.readInt) yield new UserInfo(in.readInt, in.readUTF, in.readUTF)
    }
	}

	def writeProperties:String = {
		"UserSettings {"+groupList.valuesIterator.map{_.writeString}.mkString("\n")+"}"
	}

	private def internGetValue[T ](groupName:String,name:String,defaultValue:T):T = {
		if(groupList.contains(groupName)) {
			val group=groupList(groupName)
			if(group.properties.contains(name)) try { 
				group.properties(name).value.asInstanceOf[T]
			} catch {case e:Exception => defaultValue }
			else defaultValue
		} else defaultValue
	}

	def getStringProperty(group:String,name:String,defaultValue:String=""):String = 
		internGetValue[String](group,name,defaultValue)	

	def getIntProperty(group:String,name:String,defaultValue:Int=0):Int =
			internGetValue[Int](group,name,defaultValue)

	def getListProperty[T](group:String,name:String,defaultValue:Seq[T]=Seq[T]()): Seq[T] = 
				internGetValue[collection.immutable.Seq[T]](group,name,defaultValue)
				
				

	private def getGroup(group:String):PropertyGroup	= {
		if (groupList.contains(group)) groupList(group)
		else {
			val newGroup=new PropertyGroup(group,mutable.HashMap())
			groupList(group)= newGroup
			newGroup
		}
	}

	def setStringProperty(group:String,name:String,value:String) =
		getGroup(group).properties(name)=new StringValue(name,value)

	def setIntProperty(group:String,name:String,value:Int) =
		getGroup(group).properties(name)=new IntValue(name,value)

	def setListProperty[T](group:String,name:String,value:collection.Seq[T]) =
		if(value.nonEmpty)
		getGroup(group).properties(name)=new ListValue[T](name,value)	

	def removeProperty(group:String,name:String) = getGroup(group).properties.remove(name)
	
	def parse(s: String) = {
		lazy val emptyMap=mutable.HashMap[String,PropertyGroup]()
    groupList=if(s.length==0) emptyMap  
    	else {
    		SettingsParser.parseAll(SettingsParser.allSettings, s) match {
            case SettingsParser.Success(x, _) => x
            case SettingsParser.NoSuccess(err, next) =>
              print("Failure when parsing "+
"(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
err + "\n" + next.pos.longString)
              emptyMap
        }
    	}
  }

}


object UserSetting{
  def quoteString(s: String) = {
    val charCount = s.codePointCount(0, s.length)
    "\"" + (0 until charCount).map { idx =>
      s.codePointAt(s.offsetByCodePoints(0, idx)) match {
        case 0x0d => "\\r"
        case 0x0a => "\\n"
        case 0x09 => "\\t"
        case 0x22 => "\\\""
        case 0x2f => "\\/"
        case 0x5c => "\\\\"
        case c => quoteChar(c)
      }
    }.mkString("") + "\""
  }

  def quoteChar(value: Int) = {
    value match {
      case c if c > 0xffff =>
        val chars = Character.toChars(c)
        f"\\u${chars(0).toInt}%04x\\u${chars(1).toInt}%04x"
      case c if c > 0x7e => f"\\u${c.toInt}%04x"
      case c => c.toChar
    }
  }

  object SettingsParser extends JavaTokenParsers {
    def intNumber: Parser[Int] = """-?\d+""".r ^^ { case x => x.toInt }

    def unicode: Parser[String] = rep1("\\u" ~> """[a-fA-F0-9]{4}""".r) ^^ { stringBytes =>
      new String(stringBytes.map(Integer.valueOf(_, 16).intValue.asInstanceOf[Char]).toArray)
    }

    def escaped: Parser[String] = "\\" ~> """[\\/bfnrt"]""".r ^^ { charStr =>
      val char = charStr match {
        case "r" => '\r'
        case "n" => '\n'
        case "t" => '\t'
        case "b" => '\b'
        case "f" => '\f'
        case x => x.charAt(0)
      }
      char.toString
    }

    def characters: Parser[String] = """[^\"[\x00-\x1F]\\]+""".r

    def string: Parser[String] =
      "\"" ~> rep(unicode | escaped | characters) <~ "\"" ^^ { list =>
        list.mkString("")
      }

    def reference: Parser[Reference] = (("(" ~> intNumber ) ~ ("," ~> intNumber) <~ ")") ^^ {
      case typ ~ inst => new Reference(typ,inst)
    }

    def tuple: Parser[(Int, Int)] = (("(" ~> intNumber ) ~ (":" ~> intNumber) <~ ")") ^^ {
      case typ ~ inst => (typ,inst)
    }

    def intVal:Parser[IntValue] = ("{" ~> (string <~ ":") ~ (intNumber <~"}")) ^^ {
      case name ~ value => new IntValue (name,value)
    }

    def stringVal:Parser[StringValue] = ("{" ~> (string <~ ":") ~ (string <~"}")) ^^ {
      case name ~ value => new StringValue (name,value)
    }

    def intListVal: Parser[ListValue[Int]] = ("{" ~> (string <~ ": [") ~ repsep(intNumber, ",") <~ "] }") ^^ {
      case name ~ list =>	new ListValue(name,list)
    }

    def stringListVal: Parser[ListValue[String]] = ("{" ~> (string <~ ": [") ~ repsep(string, ",") <~ "] }") ^^ {
      case name ~ list => new ListValue(name,list)
    }

    def refListVal: Parser[ListValue[Reference]] = ("{" ~> (string <~ ": [") ~ repsep(reference, ",") <~ "] }") ^^ {
      case name ~ list => new ListValue(name,list)
    }

    def tupleListVal: Parser[ListValue[(Int, Int)]] = ("{" ~> (string <~ ": [") ~ repsep(tuple, ",") <~ "] }") ^^ {
      case name ~ list => new ListValue(name,list)
    }

    def groupListVal: Parser[ListValue[PropertyGroup]]=("{" ~> (string <~ ": [") ~ repsep(propertyGroup, ",") <~ "] }") ^^ {
      case name ~ list =>	new ListValue(name,list)
    }

    def propertyGroup:Parser[PropertyGroup] = ("{" ~> (string <~ ":") ~
      (rep(intVal|stringVal|intListVal|stringListVal|refListVal|tupleListVal|groupListVal) <~"}")) ^^ {
      case name ~ list =>
        val n=mutable.HashMap[String,PropertyValue](list.map(a => (a.name , a)) : _*)
        //val hm=new collection.mutable.HashMap[String,PropertyValue]()++m
        new PropertyGroup(name,n)
    }

    def allSettings:Parser[mutable.HashMap[String,PropertyGroup]] = ("UserSettings {" ~> rep(propertyGroup)) <~ "}" ^^ {
      case list =>
        mutable.HashMap[String,PropertyGroup](list.map(a => (a.name,a)) : _*)
    }
  }

}