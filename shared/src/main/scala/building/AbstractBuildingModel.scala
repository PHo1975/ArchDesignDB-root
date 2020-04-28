package building

trait AbstractBuildingModel {

  def getPlane(id:Int):Plane

  def getRoom(id:Int):Option[Room]

  def getCell(id:Int): Cell
}
