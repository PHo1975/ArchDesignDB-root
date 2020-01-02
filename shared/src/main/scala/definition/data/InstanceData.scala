/**
  * Author: Peter Started:25.07.2010
  */
package definition.data

import java.io.{DataInput, DataOutput}

import definition.comm.KeyAble
import definition.expression.{Constant, EMPTY_EX, Expression}
import definition.typ._
import util.{Log, StringUtils}

import scala.collection.IndexedSeqView
import scala.util.control.NonFatal
import scala.util.matching.Regex

/** Holds the data of a certain Instance
  *
  *
  */
class InstanceData(override val ref: Reference, val fieldData: IndexedSeq[Expression],
                   val owners: Array[OwnerReference] = Array(), val secondUseOwners: Array[OwnerReference] = Array.empty, val hasChildren: Boolean = false)
  extends Referencable with KeyAble[Reference] {

  lazy val fieldValue: Seq[Constant] = regenFieldCache
  lazy val theClass: AbstractObjectClass = getObjectClass
  lazy val shortFormArray: Seq[Any] = getFormatArray(theClass.shortFormat.fields).toSeq
  lazy val resultFormArray: Seq[Any] = getFormatArray(theClass.resultFormat.fields).toSeq

  override def toString: String =
    if (theClass.shortFormat != NOFORMAT && fieldData.nonEmpty)
      try {
        theClass.shortFormat.formStr.format(shortFormArray: _*)
      }
      catch {
        case e: Exception => util.Log.e("Formatting " + theClass.shortFormat + " shortArray:" + shortFormArray.mkString, e); e.toString
      }
    else theClass.name + " " + ref.sToString

  override def equals(other: Any): Boolean = other match {
    case e: InstanceData => ref.equals(e.ref) && InstanceData.compareLists(fieldData, e.fieldData) &&
      InstanceData.compareArrays(owners, e.owners) &&
      InstanceData.compareArrays(secondUseOwners, e.secondUseOwners) && hasChildren == e.hasChildren
    case _ => false
  }

  override def hashCode: Int = ref.hashCode

  /** creates an Array of the current values of the fields that make up an format
    *
    * @param fieldIndexes array of the field numbers that are part of the format
    * @return an array of the current native values of the given fields
    */
  def getFormatArray(fieldIndexes: Array[Int]): IndexedSeqView[Any] =
    for (i <- fieldIndexes.view)
      yield if (i > -1) {
        val theVal = fieldValue(i)
        theVal.getNative
      } else ref.instance

  def resultString: String = if (theClass.resultFormat != NOFORMAT)
                               try {
                                 theClass.resultFormat.formStr.format(resultFormArray: _*)
                               }
                               catch {
                                 case e: Exception => "ResultString" + e.toString
                               }
                             else ""

  def writeWithChildInfo(file: DataOutput): Unit = {
    write(file)
    file.writeBoolean(hasChildren)
  }

  override def write(file: DataOutput): Unit = {
    writeFields(file)
    // owners
    file.writeByte(owners.length)
    for (owner <- owners)
      owner.write(file)
    // secondUseOwners
    file.writeShort(secondUseOwners.length)
    for (so <- secondUseOwners)
      so.write(file)
  }

  def writeFields(file: DataOutput): Unit = {
    file.writeByte(fieldData.length)
    for (field <- fieldData) {
      field.write(file)
    }
  }

  /** changes the value of a single field and returns a new instance object with the new value
    *
    * @param fieldNr  number of the field to change
    * @param newValue new expression value
    * @return a new instance object with the new value
    */
  def setField(fieldNr: Byte, newValue: Expression): InstanceData =
    new InstanceData(ref, for (i <- fieldData.indices)
      yield if (i == fieldNr) newValue else fieldData(i), owners, secondUseOwners, hasChildren)

  def addField(atPos: Int): InstanceData =
    new InstanceData(ref, if (atPos == -1) fieldData :+ EMPTY_EX
                          else {
                            val (start, end) = fieldData.splitAt(atPos)
                            (start :+ EMPTY_EX) ++ end
                          }, owners, secondUseOwners, hasChildren)

  def changeOwner(newOwners: Array[OwnerReference]): InstanceData = new InstanceData(ref, fieldData, newOwners, secondUseOwners, hasChildren)

  def addSecondUseOwner(newOwner: OwnerReference): InstanceData = changeSecondUseOwners(secondUseOwners :+ newOwner)

  def changeSecondUseOwners(newOwners: Array[OwnerReference]): InstanceData = new InstanceData(ref, fieldData, owners, newOwners, hasChildren)

  def setFieldValues(newValues: IndexedSeq[Expression]): InstanceData =
    if (newValues.size == fieldData.size) new InstanceData(ref, newValues, owners, secondUseOwners, hasChildren)
    else throw new IllegalArgumentException("wrong number of fields " + ref + " : " + newValues.size + " expected:" + fieldData.size)

  def setHasChildren(newValue: Boolean) = new InstanceData(ref, fieldData, owners, secondUseOwners, newValue)

  /** creates a copy of this instance
    * a helper routine for copying instances.
    * At the moment it creates a clone with the same class version.
    * There could be another version converting an instance from an old version to a new one,
    * having a translation list for fields.
    *
    * @param newRef    the reference of the clone instance
    * @param newOwners the owners of the new instance
    * @return
    */
  def clone(newRef: Reference, newOwners: Array[OwnerReference], newSecondUseOwners: Array[OwnerReference]): InstanceData =
    new InstanceData(newRef, fieldData.map(_.createCopy()), newOwners, newSecondUseOwners, hasChildren)


  /** replaces an ownerReferene with another ref and returns a new Instance with the new values
    *
    * @param fromRef the old ref to remove
    * @param toRef   the new ref that replaces the old one
    */
  def changeSingleOwner(fromRef: OwnerReference, toRef: OwnerReference): InstanceData = {
    val newOwnerList = for (ref <- owners)
      yield if (ref == fromRef) toRef else ref
    new InstanceData(ref, fieldData, newOwnerList, secondUseOwners, hasChildren)
  }

  def getObjectClass: AbstractObjectClass = AllClasses.get match {
    case null => UNDEFINED_CLASS
    case ac => ac.getClassByID(ref.typ)
  }

  //def printFields: String = fieldData.indices.map(ix => ix + ":" + fieldData(ix).getTerm).mkString("\n")

  // keyable interface
  def key: Reference = ref

  def toJSON: String = {
    import StringUtils.escapeJSON
    (for (fieldIx <- theClass.fields.indices; fieldSetting = theClass.fieldSetting(fieldIx)
          if fieldSetting.visible; field = theClass.fields(fieldIx)) yield
      escapeJSON(field.name) + ":" + escapeJSON(
        if (fieldSetting.showFormula) fieldData(fieldIx).getTerm
        else fieldValue(fieldIx).toString)).mkString("{", ",", "}")
  }

  def containsString(st: String, checkNumbers: Boolean): Boolean = fieldValue.exists(_.containsString(st, checkNumbers)) || fieldData.exists(_.containsString(st, checkNumbers))

  /** creates a list of current Values of all fields
    *
    * @return list of current constant values of all fields
    */
  protected def regenFieldCache: Seq[Constant] = if (theClass != UNDEFINED_CLASS) for (index <- fieldData.indices) yield try {
    val fieldType = theClass.fields(index).typ
    val result = fieldData(index).getValue
    if (result == null) {
      util.Log.e("result== null " + ref + " field:" + index + " " + fieldData(index))
      EMPTY_EX
    } else if (fieldType == DataType.undefined || result.getType == fieldType || result == EMPTY_EX)
             result // return the value
    else // return converted value
      Constant.createConversion(result, fieldType)
  } catch {
    case NonFatal(e) => util.Log.e("inst:" + ref + " field:" + index, e); EMPTY_EX
  } else for (index <- fieldData.indices) yield
    try {fieldData(index).getValue} catch {case NonFatal(e) => util.Log.e("inst:" + ref + " field:" + index, e); EMPTY_EX}
}


object InstanceData {
  val transMan: ATransactionManager = null
  // reads an instance from a DataInput

  def compareLists[T](a: Seq[T], b: Seq[T]): Boolean = if (a == null || b == null) false
                                                       else if (a.size != b.size) false
                                                       else !a.indices.exists(ix => a(ix) != b(ix))

  def compareArrays[T](a: Array[T], b: Array[T]): Boolean = if (a == null || b == null) false
  else if (a.length != b.length) false
  else !a.indices.exists(ix => a(ix) != b(ix))


  def read(nref: Reference, file: DataInput, nhasChildren: Boolean) =
   new InstanceData(nref, readFields(file), readOwners(file), readSecondUseOwners(file), nhasChildren)

  def readWithChildInfo(nref: Reference, file: DataInput): InstanceData = try {
    new InstanceData(nref, readFields(file), readOwners(file), readSecondUseOwners(file), file.readBoolean)
  } catch {
    case NonFatal(e) => throw new IllegalArgumentException(" when reading :" + nref + " " + e.toString)
  }

  def readFields(file: DataInput): IndexedSeq[Expression] = {
    val nfields = file.readByte
    for (_ <- 0 until nfields) yield Expression.read(file)
  }

  def readOwners(file: DataInput): Array[OwnerReference] = {
    val nOwners = file.readByte
    val ownArray = new Array[OwnerReference](if (nOwners < 0) 0 else nOwners)
    if (nOwners < 0) Log.e("negative num of Owners " + nOwners + " when reading owners")
    else for (o <- 0 until nOwners)
      ownArray(o) = OwnerReference.read(file)
    //println("read owers:"+ownArray.mkString(">"))
    ownArray
  }

  def readSecondUseOwners(file: DataInput): Array[OwnerReference] = {
    val nOwners = file.readShort
    //println("num second use:"+nOwners)
    val ownArray = new Array[OwnerReference](nOwners)
    for (o <- 0 until nOwners)
      ownArray(o) = OwnerReference.read(file)
    ownArray
  }

  def emptyInstance(nref:Reference)=new InstanceData(nref,IndexedSeq.empty,Array.empty,Array.empty,false)


}


/* describes one owner of an instance data
 * 
 */
@SerialVersionUID(21543L) case class OwnerReference(ownerField: Byte, //in what property field of the owner instance
                                                    //is this instance stored
                                                    ownerRef: Reference) { // reference of the owner instance

  def this(of: Int, or: Reference) = this(of.toByte, or)

  def write(out: DataOutput): Unit = {
    out.writeByte(ownerField)
    ownerRef.write(out)
  }

  override def toString: String = (if (ownerRef == null) "()" else ownerRef.sToString()) + "|" + ownerField

  def sToString: String = ownerRef.bToString() + "," + ownerField
}


object OwnerReference {
  val RMatch: Regex ="""\(?(\d+)[,](\d+)[,](\d+)\)?""".r

  def read(in: DataInput) = new OwnerReference(in.readByte, Reference(in))

  def unapply(st: String): Option[OwnerReference] = try {
    st match {
      case RMatch(otyp, oinst, prField) => Some(new OwnerReference(prField.toByte, new Reference(otyp.toInt, oinst.toInt)))
      case _ => None
    }
  } catch {case NonFatal(_) => None}
}


object OwnerRefList {
  def unapply(st: String): Option[Array[OwnerReference]] = try {
    Some(st.split(';').collect { case OwnerReference(a) => a })
  }
  catch {
    case NonFatal(e) => Log.e("Error decoding  ", e); None
  }
}


object EMPTY_OWNERREF extends OwnerReference(0, EMPTY_REFERENCE)
object EMPTY_INSTANCE extends InstanceData(EMPTY_REFERENCE,IndexedSeq.empty,Array.empty,Array.empty,false){
  override def getObjectClass: AbstractObjectClass = UNDEFINED_CLASS
}

