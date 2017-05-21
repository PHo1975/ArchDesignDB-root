/**
  * Author: Peter Started:03.09.2010
  */
package definition.comm

/** Notification Types for Subscriptions
  *
  */
object NotificationType extends Enumeration {
  val sendData = Value("Send data")
  val fieldChanged = Value("Field Changed")
  val childAdded = Value("Child Added")
  val instanceRemoved = Value("Child Removed")
  val parentNotExistend = Value("parentNotExistend")
  val updateUndo = Value("Update Undo")
  //val parentRemoved=Value("Parent Removed")

}