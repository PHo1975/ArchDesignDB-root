/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import java.io.{DataInput,DataOutput,Serializable}
import definition.typ.AllClasses
import definition.expression.{Expression, Constant}
import definition.typ.DataType
import util.Log

import scala.util.control.NonFatal

/** a reference to an instance
 * 
 */
@SerialVersionUID(24276L)case class Reference (typ:Int, instance:Int) extends Referencable// with Serializable
{
   def sToString() = "("+typ+","+instance+")"
   
   def lToString() = "("+AllClasses.get.getClassByID(typ).name+" #"+instance+")"

  def bToString() = typ.toString+","+instance
    
   override def write(file: DataOutput ) = { 
  	 file.writeInt(typ);file.writeInt(instance)
   }
   
   def isNull=  typ==0 && instance==0
   
  // def serialized= new SerialReference(typ,instance)
   def ref= this
   
   def compareTo(other:Reference) = if(typ<other.typ) -1 else if(typ>other.typ) 1 else
     if(instance<other.instance) -1 else if (instance>other.instance) 1 else 0
}


object Reference
{
   val RMatch="""\(?(\d+)[,\.](\d+)\)?""".r
  
	def apply(file: DataInput) =	{
	   new Reference(file.readInt,file.readInt)	
	}	
	
	def apply(str:String)= str match {
	  case RMatch(typ,inst)=>new Reference(typ.toInt,inst.toInt)
	  case _=> EMPTY_REFERENCE
	}
	
	
	def unapply(str:String)= str match {
	  case RMatch(typ,inst)=>Some(new Reference(typ.toInt,inst.toInt))
	  case _=> None
	}	
  
  lazy val ordering=new Ordering[Reference]{
    def compare(a:Reference, b:Reference)= a.compareTo(b) 
  }
	
}

object EMPTY_REFERENCE extends Reference(0,0)

object RefList {
  def unapply(st:String)= try {
    Some(st.split(';').collect{case Reference(a)=>a})
  }
  catch {
    case NonFatal(e)=>Log.e("Error decoding ",e);None
  }
}

