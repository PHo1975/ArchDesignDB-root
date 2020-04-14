package building

trait AbstractBuildingModel {

  val planeMap=collection.mutable.HashMap[Int,Plane]()

  val roomMap=collection.mutable.HashMap[Int,Room]()

  def getPlane(id:Int):Plane = planeMap(id)

  def getRoom(id:Int):Option[Room]= roomMap.get(id)
}
