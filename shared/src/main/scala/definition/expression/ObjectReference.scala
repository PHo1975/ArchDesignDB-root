/**
 * Author: Peter Started:28.05.2011
 */
package definition.expression
import definition.data.Reference
import definition.typ.DataType
import java.io.{DataOutput,DataInput}

/**
 * 
 */
case class ObjectReference(typ:Int, instance:Int) extends Constant {
  def this(ref:Reference)=this(ref.typ,ref.instance)
  override def toObjectReference=new Reference(typ,instance)
  
  def toInt:Int=typ  
  def toLong:Long=instance  
  def toDouble:Double=toInt.toDouble  
  def toBoolean:Boolean =toInt>0
  def getNative:Any=this
  def getType=DataType.ObjectRefTyp
  def createCopy=new ObjectReference(typ,instance)
  def getTerm="("+typ+","+instance+")"
  
  override def write(file: DataOutput ) = {
  	file.writeByte(DataType.ObjectRefTyp.id)
  	file.writeInt(typ);file.writeInt(instance)
  }
  
  def encode="$O"+typ+";"+instance
}

object ObjectReference {
	private [expression] def apply (file: DataInput) = {
		new ObjectReference(file.readInt,file.readInt)
	}
	def apply (ref:Reference)=new ObjectReference(ref.typ,ref.instance)
	
	def decode(text:String)= {
	  val sepPos=text.indexOf(';',2)
	  val typ=text.substring(2,sepPos).toInt
	  val instEnd=CurrencyConstant.findEnd(text,sepPos+1)
	  val inst=text.substring(sepPos+1,instEnd).toInt
	  (ObjectReference(typ,inst),instEnd)
	}
}