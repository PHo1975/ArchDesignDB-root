/**
  * Author: Peter Started:18.12.2010
  */
package definition.typ

import java.io.DataInput
import definition.data.{ HolidayDefinition, InstanceData, Reference }
import definition.expression.DateConstant


/**
  *
  */
trait SystemSettings {
  def systemTypes(key: String): Int

  def enums: collection.Map[String, EnumDefinition]

  def enumByID: collection.Map[Int, EnumDefinition]

  def holidays: Seq[HolidayDefinition]

  def genIDMap: collection.Map[Int, EnumDefinition] = enums.map(a => (a._2.id, a._2))

  def getCustomSettings(folderName: String): IndexedSeq[InstanceData]

  def getClientSetting(settingName: String): String

  def getHolidays(year: Int): Map[DateConstant, String] = {
    val easter = DateConstant.getEaster(year)
    (for (h <- holidays) yield if (h.month_easter == -1) (easter.addDays(h.day), h.name)
                               else (DateConstant(h.day, h.month_easter, year), h.name)).toMap
  }
}


object SystemSettings {
  var settings: SystemSettings = _

  def apply(): SystemSettings = settings
}


abstract class ClientSystemSettings(in: DataInput) extends SystemSettings {
  val _systemTypes: collection.Map[String, Int] =
    (for (_ <- 0 until in.readInt) yield in.readUTF -> in.readInt).toMap
  val enums: Map[String, EnumDefinition] =
    (for (_ <- 0 until in.readInt) yield in.readUTF -> EnumDefinition(in)).toMap
  val clientSettingsMap: Map[String, String] = (for (_ <- 0 until in.readInt)
    yield in.readUTF -> in.readUTF).toMap
  val systemSettingsMap: Map[String, Reference] = (for (_ <- 0 until in.readInt) yield in.readUTF -> Reference(in)).toMap
  val holidays: Seq[HolidayDefinition] = for (_ <- 0 until in.readInt) yield new HolidayDefinition(in)
  val enumByID: Map[Int, EnumDefinition] = enums.values.map(a => (a.id, a)).toMap
  // read in
  var myUserID: Int = in.readInt

  def getClientSetting(settingName: String): String =
    if (clientSettingsMap.contains(settingName)) clientSettingsMap(settingName)
    else ""

  def loadChildren(ref: Reference): IndexedSeq[InstanceData]

  def getCustomSettings(folderName: String): IndexedSeq[InstanceData] = if (systemSettingsMap.contains(folderName)) {
    //println("SettingsFolder "+folderName+" "+systemSettingsMap(folderName))
    loadChildren(systemSettingsMap(folderName))
  } else IndexedSeq.empty

  def systemTypes(key: String): Int = if (_systemTypes.contains(key)) _systemTypes(key)
                                      else throw new IllegalArgumentException("cant find SystemType " + key)
}