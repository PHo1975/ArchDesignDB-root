/**
 * Author: Peter Started:21.09.2010
 */
package definition.typ

import java.io.DataOutput

import definition.expression.{EMPTY_EX, Expression}
import definition.typ.form.AbstractFormBox
import util.CollUtils

import scala.Array.canBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
 * 
 */
trait AbstractObjectClass {
	
	def name:String	
	def description:String	
	def comment:String
	def id:Int
	
	protected def ownFields:Seq[AbstractFieldDefinition]
	protected def ownPropFields:Seq[PropertyFieldDefinition] 
	protected def ownActions:Iterable[ActionTrait]
	protected def superClasses:Seq[Int]	
	protected def ownFieldSettings:Seq[FieldSetting]
	
	protected var hasResolved=false	
	val fields=new ArrayBuffer[AbstractFieldDefinition]() // list of inherited fields from the super class
	val propFields = new ArrayBuffer[PropertyFieldDefinition]()
	val superClassIDs:mutable.LinkedHashSet[Int] = mutable.LinkedHashSet()
	val actions=mutable.LinkedHashMap[String,ActionTrait]()
	val fieldEditors=mutable.LinkedHashSet[String]()

	val fieldSettingMap=mutable.LinkedHashMap[Int,FieldSetting]()
	def shortFormat:InstFormat
	def longFormat:InstFormat
	def resultFormat:InstFormat
	
	def formBox:Option[AbstractFormBox]
	def customInstanceEditor:Option[String]
	def importDescriptor:Option[String]
	
	def getDescriptionOrName= if (description ==null || description.length==0) name else description
	
	lazy val propFieldIndices=propFields.indices.map(x=> propFields(x).name -> x).toMap
	
	lazy val formatFields=fields.indices.filter(ix=> fieldSetting(ix).formatField)
		
	
	def inheritsFrom(otherClassID:Int):Boolean =	superClassIDs.contains(otherClassID)

	
	def resolveSuperFields():Unit =	if(!hasResolved) {
			for(cl <-superClasses) {
				val superClass:AbstractObjectClass= AllClasses.get.getClassByID(cl) 
				superClass.resolveSuperFields()				
				superClassIDs ++=superClass.superClassIDs							
				copySuperClassFields(superClass)				
				propFields ++= superClass.propFields 
				actions ++=superClass.actions
				fieldEditors ++=superClass.fieldEditors
			}	
			superClassIDs += id	
			// add own fields
		  fields++=ownFields
			/*for(a <- ownFields)
					if(!fields.exists(a.name==_.name)){					// not found
						fields += a
					}
					else util.Log.e("can't redefine field "+a +" in class "+name)*/

			for(s <-ownFieldSettings) {
					if(s.editor.length>0)// editor defined
						fieldEditors +=s.editor
					fieldSettingMap(s.fieldNr)=s	
				}
			propFields ++=ownPropFields
			//if(ownActions==null) util.Log.e("ownActions==null in Class "+id+" name "+name)
			ownActions.foreach(a =>if(a==null) util.Log.e("action == nul in class "+id+" name "+name)  else actions(a.name)=a)
		  hasResolved=true
		}

	
	
	
	def copySuperClassFields(superClass:AbstractObjectClass) = {
		val fieldNum = fields.size
		fields++=superClass.fields
		for(i <- superClass.fields.indices)
		  if(superClass.fieldSettingMap.contains(i))
		  	fieldSettingMap(fieldNum+i)= superClass.fieldSettingMap(i).setFieldNr(fieldNum+i)
	}
	
	def fieldSetting(fieldNr:Int):FieldSetting= 	if(fieldSettingMap.contains(fieldNr) )
		fieldSettingMap(fieldNr) else EmptySetting

	
	def getFieldSettingsList:Seq[FieldSetting]= for(i<-0 until fields.size) yield fieldSetting(i)
	
	
	override def toString = "Class "+id+" Fields:\n"+fields.mkString(",")+"\nPropFields:\n"+propFields.mkString("\n")+
	"\nSuperclasses:"+ superClassIDs.mkString(",")


	def getNum_FirstHiddenPropFields:Int = {
		for(i <-propFields.indices)
			if(!propFields(i).hidden )return i
		propFields.size
	}
	
	def getPropFieldByName(name:String):Option[PropertyFieldDefinition]=
		propFieldIndices.get(name) map (propFields(_))

	
	def emptyFieldListWithSingleField(fieldNr:Int,fieldValue:Expression)= 
	  (0 until fields.size).map(ix=>if(ix==fieldNr)fieldValue else EMPTY_EX).toArray


	def writeToStream(out:DataOutput)= {
		out.writeUTF(name)
		out.writeUTF(description)
		out.writeInt(id)
		out.writeInt(ownFields.size)
		for(f<-ownFields) CollUtils.writeToStream(out,f)
		out.writeInt(ownFieldSettings.size)
		for(fs<-ownFieldSettings)CollUtils.writeToStream(out,fs)
		out.writeInt(ownPropFields.size)
		for(op<-ownPropFields) CollUtils.writeToStream(out,op)
		out.writeInt(ownActions.size)
		for(ac<-ownActions) CollUtils.writeToStream(out,ac)
		out.writeInt(superClasses.size)
		for(s<-superClasses)
			out.writeInt(s)
		out.writeUTF(shortFormat.toString)
		out.writeUTF(resultFormat.toString)

	}
}



case class InstFormat(formStr:String, fields:Array[Int]){
	override def toString= formStr+"|"+fields.mkString(",")
}
object NOFORMAT extends InstFormat("",Array.empty) {
	override def toString=""
}

object InstFormat {
	def fromString(tx:String)= {
		if(tx=="") NOFORMAT
		else {			
			val parts=tx.split('|')
			
			if(parts.length!=2) NOFORMAT
			else 
				new InstFormat(parts(0),parts(1).split(',').map(_.toInt))
		}
	}	
	def read(node: scala.xml.NodeSeq) =if(node.text==null) NOFORMAT else fromString(node.text)
		
}
