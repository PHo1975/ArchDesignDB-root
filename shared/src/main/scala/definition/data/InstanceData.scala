/**
 * Author: Peter Started:25.07.2010
 */
package definition.data

import java.io.DataInput
import java.io.DataOutput
import definition.comm.KeyAble
import util.{Log, StringUtils}

import scala.Array.canBuildFrom
import definition.expression.Constant
import definition.expression.EMPTY_EX
import definition.expression.Expression
import definition.typ.AbstractObjectClass
import definition.typ.AllClasses
import definition.typ.DataType
import definition.typ.NOFORMAT
import scala.util.control.NonFatal
/** Holds the data of a certain Instance
 * 
 * 
 */
class InstanceData (override val ref:Reference,	val fieldData:IndexedSeq[Expression],	 									
	val owners:Array[OwnerReference]=Array(),val secondUseOwners:Seq[OwnerReference]=Seq.empty,val hasChildren:Boolean=false) extends Referencable with KeyAble[Reference]
	{	
	lazy val fieldValue:Seq[Constant]=regenFieldCache
	
	lazy val theClass=getObjectClass
	lazy val shortFormArray=getFormatArray(theClass.shortFormat.fields)
	lazy val resultFormArray=getFormatArray(theClass.resultFormat.fields)
	
	override def toString = {
	  if(theClass==null) ref.sToString() else
		if(theClass.shortFormat!=NOFORMAT&&fieldData.length!=0)
			try {
			theClass.shortFormat.formStr.format(shortFormArray:_*)
			}
			catch {
				case e:Exception=>util.Log.e("Formatting "+theClass.shortFormat+" shortArray:"+shortFormArray.mkString,e); e.toString
			}
		else theClass.name+" "+ref.sToString		
	}
	
	override def equals(other:Any):Boolean = other match {
	  case e:InstanceData => ref.equals(e.ref) && InstanceData.compareLists(fieldData,e.fieldData)&& 
	    InstanceData.compareLists(owners,e.owners) &&
	    InstanceData.compareLists(secondUseOwners,e.secondUseOwners)&& hasChildren==e.hasChildren
	    
	  case _ =>false
	}
	
	override def hashCode=ref.hashCode
	
	/** creates an Array of the current values of the fields that make up an format
	 * 
	 * @param fieldIndexes array of the field numbers that are part of the format
	 * @return an array of the current native values of the given fields
	 */
	def getFormatArray(fieldIndexes:Array[Int]):Array[Any] = {
		for(i<-fieldIndexes) 
			yield if(i> -1){
				val theVal=fieldValue(i)
				 theVal.getNative 
			}
			else ref.instance
	}
	
	def resultString= {
		if(theClass.resultFormat!=NOFORMAT) {
			//System.out.println("resultArray:"+resultFormArray.mkString)
			//System.out.println(fieldValue(0).getType)
			try {
			theClass.resultFormat.formStr.format(resultFormArray:_*)
			}
			catch {
				case e:Exception=>"ResultString"+ e.toString
			}
			//"RESULT"
		}
			
		else ""
	}

	
	def writeFields(file:DataOutput)= {
	  file.writeByte(fieldData.length)
		for(field<-fieldData)
		{			
			field.write(file)
		}
	}

	override def write(file:DataOutput) = 	{
		writeFields(file)
		// owners
		file.writeByte(owners.length)
		for(owner<-owners)
			owner.write(file)
			// secondUseOwners
		file.writeShort(secondUseOwners.length)
		for(so<-secondUseOwners)
			so.write(file)
	}

	def writeWithChildInfo(file:DataOutput) = {
		write(file)
		file.writeBoolean(hasChildren)
	}

	/** changes the value of a single field and returns a new instance object with the new value
	 *  
	 * @param fieldNr number of the field to change
	 * @param newValue new expression value
	 * @return a new instance object with the new value
	 */
	def setField(fieldNr:Byte,newValue:Expression):InstanceData = 	{
		val newArray:IndexedSeq[Expression]= for (i <- 0 until fieldData.length)
			yield if (i == fieldNr) newValue else fieldData(i)
			new InstanceData(ref,newArray,owners,secondUseOwners,hasChildren)
	}
	
	def addField(atPos:Int):InstanceData = 	{		
			val newFields=if(atPos== -1) fieldData :+ EMPTY_EX
			else {
			  val (start,end)=fieldData.splitAt(atPos)
			  (start :+ EMPTY_EX) ++ end
			}
			new InstanceData(ref,newFields,owners,secondUseOwners,hasChildren)
	}

	def changeOwner(newOwners:Array[OwnerReference]) = {
		new InstanceData(ref,fieldData,newOwners,secondUseOwners,hasChildren)
	}
	
	def changeSecondUseOwners(newOwners:Seq[OwnerReference]) = {
		new InstanceData(ref,fieldData,owners,newOwners,hasChildren)
	}
	
	def addSecondUseOwner(newOwner:OwnerReference)= {
		changeSecondUseOwners(secondUseOwners:+ newOwner)
	}
	
	def setFieldValues(newValues:IndexedSeq[Expression])= if(newValues.size==fieldData.size) new InstanceData(ref,newValues,owners,secondUseOwners,hasChildren)
	else throw new IllegalArgumentException("wrong number of fields "+ ref+" : "+newValues.size+" expected:"+fieldData.size)
	
	

	def setHasChildren(newValue:Boolean) = {
		new InstanceData(ref,fieldData,owners,secondUseOwners,newValue)
	}

	/** creates a copy of this instance
	 * a helper routine for copying instances.
	 * At the moment it creates a clone with the same class version.
	 * There could be another version converting an instance from an old version to a new one,
	 * having a translation list for fields.
	 * 
	 * @param newRef the reference of the clone instance
	 * @param newOwners the owners of the new instance
	 * @return
	 */
	def clone(newRef:Reference,newOwners:Array[OwnerReference],newSecondUseOwners:Seq[OwnerReference]):InstanceData =	{
		new InstanceData(newRef,fieldData.map(_.createCopy()),newOwners,newSecondUseOwners,hasChildren)
	}

	/** replaces an ownerReferene with another ref and returns a new Instance with the new values
	 * 
	 * @param fromRef the old ref to remove
	 * @param toRef the new ref that replaces the old one
	 */
	def changeSingleOwner(fromRef:OwnerReference,toRef:OwnerReference):InstanceData = {
		val newOwnerList= for (ref <- owners)
			yield if (ref == fromRef) toRef else ref
			new InstanceData(ref,fieldData,newOwnerList,secondUseOwners,hasChildren)
	}


	def getObjectClass:AbstractObjectClass =	AllClasses.get match {
	    case null => null
	    case ac=>ac.getClassByID(ref.typ) 
	  }	
	

	/** creates a list of current Values of all fields
	 * 
	 * @return list of current constant values of all fields
	 */
	protected def regenFieldCache:Seq[Constant] = if(theClass!=null) for(index<-0 until fieldData.size)yield try{
			val fieldType = theClass.fields(index).typ
			val result=fieldData(index).getValue
			if(result==null) {
        util.Log.e("result== null "+ref+" field:"+index+" "+fieldData(index))
				EMPTY_EX
			} else			 			
			if(fieldType==DataType.undefined || result.getType==fieldType || result==EMPTY_EX )
				result // return the value
				else  // return converted value
					Constant.createConversion(result,fieldType)						  
	} catch {
    case NonFatal(e)=> util.Log.e("inst:"+ref+" field:"+index,e);EMPTY_EX
  } else 	for(index<-0 until fieldData.size) yield
      try{fieldData(index).getValue}catch {case NonFatal(e)=>util.Log.e("inst:"+ref+" field:"+index,e);EMPTY_EX}
	
	def printFields=fieldData.indices.map(ix=>ix+":"+fieldData(ix).getTerm).mkString("\n")
  
  // keyable interface
  def key=ref

  def toJSON= {
    import StringUtils.escapeJSON
    (for(fieldIx <-theClass.fields.indices;fieldSetting=theClass.fieldSetting(fieldIx)
				 if fieldSetting.visible;field=theClass.fields(fieldIx)) yield
        escapeJSON(field.name)+":"+escapeJSON(
          if(fieldSetting.showFormula) fieldData(fieldIx).getTerm
          else fieldValue(fieldIx).toString)).mkString("{",",","}")
  }
}


object InstanceData 
{
	val transMan:ATransactionManager=null
	// reads an instance from a DataInput

	def readFields(file:DataInput):IndexedSeq[Expression]= {
		val nfields=file.readByte
		for(i <-0 until nfields) yield Expression.read(file)
		//println("read fields "+result.zipWithIndex.mkString("|"))
		//result
	}

def readOwners(file:DataInput) = {
	val nOwners=file.readByte
	val ownArray=new Array[OwnerReference](if(nOwners<0) 0 else nOwners)
	if(nOwners<0) Log.e("negative num of Owners "+nOwners+" when reading owners")
	else for( o<- 0 until nOwners)
		ownArray(o)= OwnerReference.read(file)
	//println("read owers:"+ownArray.mkString(">"))
	ownArray
}

def readSecondUseOwners(file:DataInput) = {
	val nOwners=file.readShort
	//println("num second use:"+nOwners)
	val ownArray=new Array[OwnerReference](nOwners)
	for( o<- 0 until nOwners)
		ownArray(o)= OwnerReference.read(file)
	ownArray		
}

def compareLists[T](a:Seq[T],b:Seq[T]):Boolean= if(a==null || b==null) false
	else if(a.size!=b.size) false
	else ! a.indices.exists(ix=> a(ix)!= b(ix))


def read(nref:Reference,file:DataInput,nhasChildren:Boolean) = 
	/*if(nref.isNull) new InstanceData(nref,Array(),Array(),false)
	else*/ new InstanceData(nref,readFields(file),readOwners(file),readSecondUseOwners(file),nhasChildren)	

def readWithChildInfo(nref:Reference,file:DataInput)= try {
	new InstanceData(nref,readFields(file),readOwners(file),readSecondUseOwners(file),file.readBoolean)
} catch {
  case NonFatal(e)=> throw new IllegalArgumentException(" when reading :"+nref+" "+e.toString)
}



}

/* describes one owner of an instance data
 * 
 */
@SerialVersionUID(21543L) case class OwnerReference(ownerField:Byte, //in what property field of the owner instance
	//is this instance stored   
																										ownerRef:Reference) { // reference of the owner instance
  
  def this(of:Int,or:Reference)=this(of.toByte,or)
  
	def write(out:DataOutput) = {
		out.writeByte(ownerField)
		ownerRef.write(out)
	}
	override def toString= (if (ownerRef == null) "()" else ownerRef.sToString()) + "|" + ownerField
	def sToString=ownerRef.bToString()+","+ownerField
}

object OwnerReference {
	val RMatch="""\(?(\d+)[,](\d+)[,](\d+)\)?""".r
	def read(in:DataInput) = new OwnerReference(in.readByte,Reference(in))

	def unapply(st:String):Option[OwnerReference]= try {
		st match {
			case RMatch(otyp, oinst, prField) => Some(new OwnerReference(prField.toByte, new Reference(otyp.toInt, oinst.toInt)))
			case _=> None
		}
	} catch {case NonFatal(e)=>None}
}

object OwnerRefList {
	def unapply(st:String)= try {
		Some(st.split(';').collect{case OwnerReference(a)=>a})
	}
	catch {
		case NonFatal(e)=>Log.e("Error decoding  ",e);None
	}
}

object EMPTY_OWNERREF extends OwnerReference(0,EMPTY_REFERENCE)


