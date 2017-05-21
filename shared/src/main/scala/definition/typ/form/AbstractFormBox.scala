/**
  * Author: Peter Started:13.04.2011
  */
package definition.typ.form


import definition.data.InstanceData
import definition.typ.AbstractObjectClass

/** trait for accessing the addBox of the Designer
  *
  */
trait AbstractSpecialComponent {
  def formBox: AbstractFormBox
}


/**
  *
  */
trait AbstractFormBox extends AbstractFormElement {
  def horizontalOrient: Boolean

  def elements: Seq[AbstractFormElement]

  def specialComp: Option[AbstractSpecialComponent]

  def addElement(newElement: AbstractFormElement, pos: Int = -1): AbstractFormBox

  def setSpecialComp(s: AbstractSpecialComponent): Unit

  def updateElement(pos: Int, newElement: AbstractFormElement): AbstractFormBox

  def updateElement(oldValue: AbstractFormElement, newValue: AbstractFormElement): (AbstractFormBox, Boolean)

  def deleteElement(elem: AbstractFormElement): (AbstractFormBox, Boolean)

  def foreach(s: (AbstractFormElement) => Unit): Unit

  def setDataValue(dvalue: InstanceData, nclass: AbstractObjectClass): Unit

  def setListener(nlist: Option[DataChangeListener]): Unit

  def findFormBoxFor(el: AbstractFormElement): (AbstractFormBox, Int)
}

