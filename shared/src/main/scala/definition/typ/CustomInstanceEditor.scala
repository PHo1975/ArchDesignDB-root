/**
  * Author: Peter Started:22.05.2011
  */
package definition.typ

//import scala.swing.Component
import definition.data.Reference

/** an editor component to be shown in the table browser for a certain kind of class
  *
  */
trait CustomInstanceEditor[A] {
  def getComponent: A

  def load(ref: Reference, loadDoneListener: () => Unit): Unit

  def shutDown(): Unit

  def editorName: String

  //def setSizeChangeListener(listener:Function0[Unit])= {}
  def fullSize: Boolean = false
}