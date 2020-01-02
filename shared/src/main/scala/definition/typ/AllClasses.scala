/**
  * Author: Peter Started:26.06.2010
  */
package definition.typ

import definition.data.{OwnerReference, Referencable, Reference}
import util.Log

import scala.collection.mutable

/**
  * Contains a list of all current classes
  */
abstract class AllClasses[B <: AbstractObjectClass] /*(node: scala.xml.Node)*/ {

  def classList: collection.Map[Int, B]

  def blockClassList:collection.Map[Int,BlockClass]

  // get the Map object containing all classes
  def getClassList: collection.Map[Int, B] = classList

  def getClassIDByName(aname: String): Int = getClassByName(aname) match {
    case Some(aclass) => aclass.id
    case _ => throw new IllegalArgumentException("Class " + aname + " not found !")
  }

  // find a class by Name
  def getClassByName(aname: String): Option[AbstractObjectClass] = classList.valuesIterator.find(_.name == aname)

  def getBlockClassByName(aname:String): Option[BlockClass] =blockClassList.valuesIterator.find(_.name==aname)

  // resolves all superfields from all classes. Will be called after reading all classes from XLM
  def resolveFields(): Unit = for (cl <- classList.valuesIterator)
    cl.resolveSuperFields()

  //def fromXML(node: scala.xml.Node):Map[Int,B]

  /** gets the most common class that all classes inherit from
    *
    * @param dataList list of referencable Instances
    * @return the id of the common class or -1 if there is no common class
    */
  def getCommonClass(dataList: Seq[Referencable]): Int = {
    if (dataList == null || dataList.isEmpty) return -1
    if (dataList.size == 1) return dataList.head.ref.typ
    var aClassID = -2
    var superClasses: mutable.LinkedHashSet[Int] = null
    for (inst <- dataList) {
      if (aClassID == -2) {
        aClassID = dataList.head.ref.typ
        superClasses = getClassByID(aClassID).superClassIDs
      }
      else if (inst.ref.typ != aClassID) {
        val otherSuperClasses = getClassByID(inst.ref.typ).superClassIDs
        superClasses = superClasses intersect otherSuperClasses
        if (superClasses.isEmpty) return -1
        aClassID = superClasses.last
      }
    }
    aClassID
  }

  // find a class by class ID
  def getClassByID(aId: Int): B = if (classList.contains(aId)) classList(aId)
                                  else {
                                    Log.e("getclassbyID id not found " + aId, Thread.currentThread().getStackTrace)
                                    throw new IllegalArgumentException("getClassByID: ID " + aId + " not found size:" + classList.size)
                                  }

  def getCommonClassForGroups(groupList: Iterable[SelectGroup[_ <: Referencable]]): Int = {
    if (groupList == null || groupList.isEmpty) -1
    else if (groupList.size == 1 && groupList.head.children.size == 1) {
      val typ = groupList.head.children.head.ref.typ
      if (typ == 0) -1 else typ
    } else {
      var aClassID = -2
      var superClasses: mutable.LinkedHashSet[Int] = null
      for (group <- groupList)
        for (inst <- group.children) {
          if (aClassID == -2) {
            aClassID = inst.ref.typ
            superClasses = getClassByID(aClassID).superClassIDs
          }
          else if (inst != null && inst.ref.typ != aClassID) {
            val otherSuperClasses = getClassByID(inst.ref.typ).superClassIDs
            superClasses = superClasses intersect otherSuperClasses
            if (superClasses.isEmpty) return -1
            aClassID = superClasses.last
          }
        }
      aClassID
    }
  }

  def getDirectSuccessorsFor(classID: Int): Iterator[B] =
    classList.valuesIterator.filter(cl => cl.id != classID && cl.superClassIDs.contains(classID))

  def hasSubClasses(classID: Int): Boolean =
    classList.valuesIterator.exists(cl => cl.id != classID && cl.superClassIDs.contains(classID))

}


case class SelectGroup[T <: Referencable](var parent: OwnerReference, var children: Iterable[T]) {
  override def toString: String = "SelectGroup parent:" + parent + " children:" + children.mkString("| ")
}


object AllClasses {
  var classObj: AllClasses[_ <: AbstractObjectClass] = _

  def get: AllClasses[_ <: AbstractObjectClass] = classObj

  def set(newObj: AllClasses[_ <: AbstractObjectClass], resolve: Boolean = true): Unit = {
    classObj = newObj
    // Resove Superfields in all ClassVersions
    if (resolve) classObj.resolveFields()
  }

  def stringToIntList(text: String): Array[Int] =
    if (text == null || text.length == 0) Array.empty
    else text.split(",").map(_.toInt)

  def refToString(ref: Reference): String = if (ref == null) "null" else if (ref.typ < 1) "[Undefined,ref.instance]"
  else "[" + classObj.getClassByID(ref.typ).name + "," + ref.instance + "]"
}