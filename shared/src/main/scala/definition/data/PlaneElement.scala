/**
  * Author: Peter Started:22.05.2011
  */
package definition.data

import definition.expression.{Plane3D, VectorConstant}


/**
  *
  */


case class PlaneElement(ref: Reference, owners: Array[OwnerReference], suOwners: Seq[OwnerReference], name: String,
                        npos: VectorConstant, ndir: VectorConstant, color: Int) extends
  Plane3D(npos, ndir) with Referencable with Named {

  /** creates a new plane that is parallel to this plane, with a distance of offset toward the given point
    *
    * @param towards in what direction
    * @param offset  offset measure, distance to the new plae
    */
  def createOffsetPlane(towards: VectorConstant, offset: Double): PlaneElement = {
    val orth = orthogonalThrough(towards).unit * offset
    createClone(pos + orth, dir)
  }

  override def createClone(newPos: VectorConstant, newDir: VectorConstant): PlaneElement =
    PlaneElement(ref, owners, suOwners, name, newPos, newDir, color)

  override def toString: String = "Plane " + name + " " + ref

}


