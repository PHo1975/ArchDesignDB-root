/**
  * Author: Peter Started:29.08.2010
  */
package definition.comm

/** commands that are send by the server
  *
  */
object ServerCommands extends Enumeration {
  val sendTypes = Value("send Types")
  val sendSystemSettings = Value("send system Settings")
  //val getSetupData=Value("set Setup Data")
  val sendQueryResponse = Value("sendQueryResponse")
  val sendFactQueryResponse = Value("sendFactQueryResponse")
  val sendCommandResponse = Value("sendCommandResponse")
  val sendUserSettings = Value("send user settings")
  //val sendOrderedData=Value("send ordered Data")
  val acceptSubscription = Value("Accept subscription")
  //val acceptPathSubscription=Value("Accept path subscription")
  val sendSubscriptionNotification = Value("send notification")
  //val sendPathSubsNotification=Value("send path notification")
  val wantQuit = Value("want quit")
  val lockForUndo = Value("Lock for Undo")
  val releaseUndoLock = Value("Release Undo Lock")
  val sendUndoInformation = Value("Send Undo information")
  val askEnquiry = Value("Ask Enquiry")
  val sendGeneratedData = Value("Send generated Data")
  val sendCalendarData = Value("CalendarData")
  val sendSearchResult = Value("SendSearchResult")
  val acceptBlockSubscription=Value("Accept block subscription")
  //val sendBlockSubscriptionNotification=Value("send block subscription notification")
}


object GeneratorType extends Enumeration {
  val printGenerator = Value("Print Generator")
}