/**
  * Author: Peter Started:29.08.2010
  */
package definition.comm

/** commands that are send by clients
  *
  */
object ClientCommands extends Enumeration {
  val getTypes = Value("Get Types")
  val getSystemSettings = Value("Get System settings")
  //val getSetupData=Value("Get Setup Data")
  val storeSetupData = Value("Store Setup Data")
  val queryInstance = Value("Query Instance")
  val factQueryInstance = Value("Factory Query Instance")
  val startSubscription = Value("Start Subscription")
  val changeSubscription = Value("Change Subscription")
  val stopSubscription = Value("Stop Subscription")
  val pauseSubscription = Value("Pause Subscription")
  val startPathSubscription = Value("Start Path Subscription")
  val pathSubs_openChild = Value("open Child")
  val pathSubs_jumpUp = Value("Jump up")
  val pathSubs_changePath = Value("Change Path")
  val createInstance = Value("create Instance")
  val writeField = Value("write Field")
  val writeInstance = Value("write Instance")
  val createInstances = Value("create Instances")
  val writeMultiFields = Value("write Field of multiple instances")
  val deleteInstance = Value("delete Instance")
  val copyInstances = Value("copy Instances")
  val moveInstances = Value("move Instances")
  val secondUseInstances = Value("secondUse Instances")
  val executeAction = Value("execute Action")
  val executeCreateAction = Value("execute Action")
  val logOut = Value("logout")
  val getUserSettings = Value("get user settings")
  val writeUserSettings = Value("write user settings")
  val writeKeyStrokes = Value("write Keystrokes")
  val requestUndoData = Value("Request undo data")
  val undo = Value("Undo transaction")
  val stopUndo = Value("Stop undo session")
  val answerEnquiry = Value("Answer Enquiry")
  val getNextParentOfType = Value("Next Parent Of Type")
  val searchForText = Value("Search for Text")
  val convertInstances = Value("Convert Instances")
  val createBlock=Value("Create block")
  val changeBlock=Value("Change block")
  val deleteBlock=Value("Delete block")
  val startBlockSubscription=Value("startBlockSubscription")
}