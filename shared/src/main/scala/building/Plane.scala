package building

import definition.data.{Named, Referencable, Reference}
import definition.expression.{Constant, Plane3D}

case class Plane(ref:Reference,name:String,plane:Plane3D) extends Referencable with Named {
  def this(nref:Reference,ndata:Seq[Constant])=this(nref,ndata(0).toString,new Plane3D(ndata(1).toVector,ndata(2).toVector))


}

case class Room(ref:Reference,name:String) extends Referencable {
  def this(nref:Reference,ndata:Seq[Constant])=this(nref,ndata(0).toString)
}


