/**
  * Author: Peter Started:04.10.2010
  */
package definition.expression

import definition.typ.DataType

import java.io.{DataInput, DataOutput}
import scala.collection.mutable.ArrayBuffer

/** a 3D Vector
  *
  */
case class VectorConstant(x: Double, y: Double, z: Double) extends Constant {

  import VectorConstant._

  def this() = this(0d, 0d, 0d)

  def getType: DataType.Value = DataType.VectorTyp

  override def toString: String = getTerm

  def getTerm: String = "V[" + x + ";" + y + ";" + z + "]"

  def toStringXY: String = "[" + x + ";" + y + "]"

  def toInt: Int = toDouble.toInt

  def toLong: Long = toDouble.toLong

  def toBoolean: Boolean = x != 0 || y != 0 || z != 0

  def write(file: DataOutput): Unit = {
    file.writeByte(DataType.VectorTyp.id)
    file.writeDouble(x)
    file.writeDouble(y)
    file.writeDouble(z)
  }

  def isNull: Boolean = (x == 0d) && (y == 0d) && (z == 0d)

  def getNative: String = shortToString

  def shortToString: String = "[" + x + ";" + y + ";" + z + "]"

  override def toVector: VectorConstant = this

  def encode: String = "$V(" + x + ";" + y + ";" + z + ")"

  override def equals(other: Any): Boolean =
    other match {
      case that: VectorConstant =>
        (that canEqual this) && x == that.x && y == that.y && z == that.z
      case _ => false
    }

  def canEqual(other: Any): Boolean = other.isInstanceOf[VectorConstant]

  override def hashCode: Int = (x + 41 + 41 * (y + 1) + 554 * (z + 1)).toInt

  def transposeXY = new VectorConstant(-y, x, z)

  def +(other: VectorConstant) = new VectorConstant(x + other.x, y + other.y, z + other.z)

  //******************************* Vector routines  ************************************************

  def +(ox: Double, oy: Double, oz: Double) = new VectorConstant(x + ox, y + oy, z + oz)

  def <(other: VectorConstant): Boolean = {
    val rx = math.round(x * tolFact)
    val ox = math.round(other.x * tolFact)
    if (rx != ox) rx < ox else {
      val ry = math.round(y * tolFact)
      val oy = math.round(other.y * tolFact)
      if (ry != oy) ry < oy else
        math.round(z * tolFact) < math.round(other.z * tolFact)
    }
  }

  def >(other: VectorConstant): Boolean = if (x != other.x) x > other.x else if (y != other.y) y > other.y else z > other.z

  def angleBetween(withOther: VectorConstant): Double = Math.acos(cosBetween(withOther)) * 180d / math.Pi

  def angleBetweenRad(withOther: VectorConstant): Double = {
    val angleVal = cosBetween(withOther)
    Math.acos(if (angleVal > 1) 1 else angleVal)
  }

  @inline def cosBetween(withOther: VectorConstant): Double = *(withOther) / (toDouble * withOther.toDouble)

  def toDouble: Double = Math.sqrt(x * x + y * y + z * z)

  // scalar product
  def *(other: VectorConstant): Double = x * other.x + y * other.y + z * other.z

  def XYAngle: Double = math.atan2(y, x)

  def norm2d: VectorConstant = new VectorConstant(-y, x, 0d).unit

  def unit: VectorConstant = {
    val length = toDouble
    if (length == 0d) NULLVECTOR
    else *(1d / length)
  }

  def cross(other: VectorConstant): VectorConstant =
    new VectorConstant(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x)

  def squareDistanceTo(ox: Double, oy: Double, oz: Double): Double = {
    val dx = x - ox
    val dy = y - oy
    val dz = y - oz
    dx * dx + dy * dy + dz * dz
  }

  def compareTo(nob: VectorConstant): Int = {
    if (x > nob.x) 1
    else if (x < nob.x) -1
    else if (y > nob.y) 1
    else if (y < nob.y) -1
    else if (z > nob.z) 1
    else if (z < nob.z) -1
    else 0
  }

  /** get a vector that is orthogonal to this and points to fromVector
    *
    * @param fromVector other Vector
    * @return
    */
  def orthogonalThrough(fromVector: VectorConstant): VectorConstant = this.-(orthoProjection(fromVector))

  def orthoProjection(toVector: VectorConstant): VectorConstant =
    if (toVector.toDouble == 0d) toVector
    else {
      val skal = *(toVector) / (toVector * toVector)
      toVector * skal
    }

  /** is this vector lineary dependent with the other vector
    *
    * @param other the other vector
    * @return true if both vectors are lineary dependent
    */
  def isLinearyDependentFrom(other: VectorConstant): Boolean =
    det2D(other.x, other.y, x, y) == 0 && det2D(other.x, other.z, x, z) == 0 && det2D(other.y, other.z, y, z) == 0

  def isNearlyLinearyDependentFrom(other: VectorConstant,tol:Double=tolerance): Boolean =
    Math.abs(det2D(other.x, other.y, x, y)) < tol &&
      Math.abs(det2D(other.x, other.z, x, z)) < tol &&
      Math.abs(det2D(other.y, other.z, y, z)) < tol

  /** checks if this point is in the segment between p1 and p2
    * when this point is on the line from p1 to p2
    *
    * @param p1 first point of segment
    * @param p2 second point of segment
    * @return
    */
  def isInSegment(p1: VectorConstant, p2: VectorConstant): Boolean = dividesSegment(p1, p2) >= 0d

  /** checks if this point is in the segment between p1 and p2
    * when this point is on the line from p1 to p2
    *
    * @param p1 first point of segment
    * @param p2 second point of segment
    * @return
    */
  def isInSegmentExclusive(p1: VectorConstant, p2: VectorConstant): Boolean = dividesSegment(p1, p2) > tolerance


  /** gets the proportional scale of this point in the segment from p1 to p2
    * when this point is on the line from p1 to p2
    *
    * @param p1 first point of segment
    * @param p2 second point of segment
    * @return the scale value, so that (this-p1)=scale * (p2-this)
    */
  def dividesSegment(p1: VectorConstant, p2: VectorConstant): Double = {
    (this - p1).getScaleTo(p2 - this)
  }

  def -(other: VectorConstant) = new VectorConstant(x - other.x, y - other.y, z - other.z)

  /** gets the scale value, so that scale*other=this
    * for lineary depended vectors
    *
    * @param other other Vector
    */
  def getScaleTo(other: VectorConstant): Double = {
    if (math.abs(other.x) > VectorConstant.tolerance) x / other.x
    else if (math.abs(other.y) > VectorConstant.tolerance) y / other.y
    else if (math.abs(other.z) > VectorConstant.tolerance) z / other.z
    /*if(other.x!=0) x/other.x
    else if(other.y!=0) y/other.y
    else if(other.z!=0) z/other.z*/
    else 0d
  }

  /** distance to a ray that starts from StartPoint to direction dir
    *
    */
  def hatchDistance(dir: VectorConstant, dir2: Double, norm: VectorConstant, startPoint: VectorConstant): Double = {
    val dx = x - startPoint.x
    val dy = y - startPoint.y
    val pdir = dx * dir.x + dy * dir.y
    val nx = dx - dir.x * (pdir / dir2)
    val ny = dy - dir.y * (pdir / dir2)
    if (math.abs(norm.y) < tolerance) nx / norm.x else ny / norm.y
  }

  def revert: VectorConstant = *(-1d)

  //  scale
  def *(scale: Double) = new VectorConstant(x * scale, y * scale, z * scale)

  def flipY = new VectorConstant(x, -y, z)

  def alignValues: Option[VectorConstant] = {
    val xValue = {
      val rounded = Math.round(x * 1000000d) / 1000000d; if (x != rounded) Some(rounded) else None
    }
    val yValue = {
      val rounded = Math.round(y * 1000000d) / 1000000d; if (y != rounded) Some(rounded) else None
    }
    val zValue = {
      val rounded = Math.round(z * 1000000d) / 1000000d; if (z != rounded) Some(rounded) else None
    }
    if (xValue.isDefined || yValue.isDefined || zValue.isDefined) Some(new VectorConstant(xValue.getOrElse(x), yValue.getOrElse(y), zValue.getOrElse(z)))
    else None
  }

  def similar(b: VectorConstant): Boolean = math.abs(x - b.x) < tolerance && (math.abs(y - b.y) < tolerance) && (math.abs(z - b.z) < tolerance)

  private[expression] def this(in: DataInput) = this(in.readDouble, in.readDouble, in.readDouble)
}


/**
  * @param pos Position of the directional vector
  * @param dir Directional vector of the line
  */
case class Line3D(pos: VectorConstant, dir: VectorConstant) {

  import VectorConstant._

  lazy val dirPoint: VectorConstant = pos + dir

  def intersectionWith(other: Line3D): VectorConstant = {
    val dif = other.pos - pos
    if (dir.isLinearyDependentFrom(other.dir))
      throw new ArithmeticException("Intersection not possible, vectors are linerary dependent " + this + " " + other)
    val det = VectorConstant.determinant(dir, other.dir, dif)
    if (Math.abs(det) < tolerance) { // there is an intersection
      val det2 = det2D(dir.x, dir.y, other.dir.x, other.dir.y)
      if (det2 != 0) {
        val det3 = det2D(dif.x, dif.y, other.dir.x, other.dir.y)
        return pos + (dir * (det3 / det2))
      } else {
        val det4 = det2D(dir.x, dir.z, other.dir.x, other.dir.z)
        if (det4 != 0d) {
          val det5 = det2D(dif.x, dif.z, other.dir.x, other.dir.z)
          return pos + (dir * (det5 / det4))
        } else {
          val det6 = det2D(dir.y, dir.z, other.dir.y, other.dir.z)
          if (det6 != 0d) {
            val det7 = det2D(dif.y, dif.z, other.dir.y, other.dir.z)
            return pos + (dir * (det7 / det6))
          }
        }
      }
    }
    throw new ArithmeticException("Cant find intersection between " + this + " and " + other + " det:" + det)
  }

  def distanceTo(point: VectorConstant): Double = orthogonalThrough(point).toDouble

  def mirrorPoint(point: VectorConstant): VectorConstant = point - orthogonalThrough(point) * 2

  /** get a vector that is orthogonal to this and points to point
    *
    * @param point other point to point to
    * @return
    */
  def orthogonalThrough(point: VectorConstant): VectorConstant =
    point - orthProjection(point)

  /** orthogonal projecion from point to this line
    *
    * @param point a point in space
    * @return the projection point on this line
    */
  def orthProjection(point: VectorConstant): VectorConstant =
    pos + (point - pos).orthoProjection(dir)

  def isLinearDependent(other: Line3D): Boolean = dir.isLinearyDependentFrom(other.dir)

  /** in 2D space, checks if the point is left or right from the line
    *
    * @return <0 if point is right, =0 if point is on line, >0 if point is left
    *
    */
  def pointLocation2D(point: VectorConstant): Double = {
    VectorConstant.pointLocation2D(pos, dirPoint, point)
  }


}


object VectorConstant {
  val tolerance = 0.0000001d
  val tolFact = 10000000d
  val PI2: Double = Math.PI * 2d
  val alignTreshold = 0.00001d
  val pointOrdering: Ordering[VectorConstant] = (a: VectorConstant, b: VectorConstant) => if (a.x < b.x) -1 else if (a.x > b.x) 1 else if (a.y < b.y) -1 else if (a.y > b.y) 1 else if (a.z < b.z) -1 else if (a.z > b.z) 1 else 0
  val pointOrderingYFlip: Ordering[VectorConstant] = (a: VectorConstant, b: VectorConstant) => if (a.x < b.x) -1 else if (a.x > b.x) 1 else if (a.y > b.y) -1 else if (a.y < b.y) 1 else if (a.z < b.z) -1 else if (a.z > b.z) 1 else 0
  val upVector = new VectorConstant(0, 0, 1)

  def fromAngle2D(angle: Double) = new VectorConstant(math.cos(angle), math.sin(angle), 0)

  def read(in: DataInput): VectorConstant = {
    val typ = in.readByte
    if (typ != DataType.VectorTyp.id) System.err.print(" wrong vector typ:" + typ)
    new VectorConstant(in)
  }

  def det2D(a11: Double, a21: Double, a12: Double, a22: Double): Double = {
    a11 * a22 - a12 * a21
  }

  def similar(a: VectorConstant, b: VectorConstant): Boolean = math.abs(a.x - b.x) < tolerance && (math.abs(a.y - b.y) < tolerance) && (math.abs(a.z - b.z) < tolerance)

  def determinant(a: VectorConstant, b: VectorConstant, c: VectorConstant): Double =
    a.x * b.y * c.z - a.x * c.y * b.z - a.y * b.x * c.z + a.y * c.x * b.z + a.z * b.x * c.y - a.z * c.x * b.y


  def decode(text: String): (VectorConstant, Int) = {
    val end = text.indexOf(')', 3)
    val parts = text.substring(3, end).split(';')
    (new VectorConstant(parts(0).toDouble, parts(1).toDouble, parts(2).toDouble), end + 1)
  }

  def lineFromPoints(point1: VectorConstant, point2: VectorConstant): Line3D = Line3D(point1, point2 - point1)

  /** in 2D space, checks if the point is left or right from the line
    *
    * @return <0 if point is right, =0 if point is on line, >0 if point is left
    *
    */
  def pointLocation2D(p1: VectorConstant, p2: VectorConstant, point: VectorConstant): Double = {
    (p2.x - p1.x) * (point.y - p1.y) - (point.x - p1.x) * (p2.y - p1.y)
  }

  def kahatSum(values: List[Double]): Double = {
    var s = values.head
    var c = 0d
    for (i <- 1 until values.size) {
      val y = values(i) - c
      val t = s + y
      c = (t - s) - y
      s = t
    }
    s
  }

  def intersection2D(p1: VectorConstant, p2: VectorConstant, op1: VectorConstant, op2: VectorConstant): Option[VectorConstant] = {
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    val ody = op2.y - op1.y
    val odx = op2.x - op1.x
    val d = ody * dx - odx * dy
    if (math.abs(d) > tolerance) {
      val ua = (odx * (p1.y - op1.y) - ody * (p1.x - op1.x)) / d
      //val ub=(dx*(p1.y-op1.y)-dy*(p1.x-op1.x))/d
      val x = p1.x + ua * dx
      val y = p1.y + ua * dy
      Some(new VectorConstant(x, y, 0))
    }
    else None
  }

  def intersectInSegment2D(a1: VectorConstant, a2: VectorConstant, b1: VectorConstant, b2: VectorConstant): Option[VectorConstant] = {
    intersection2D(a1, a2, b1, b2).filter(p => p.isInSegment(a1, a2) && p.isInSegment(b1, b2))
  }

  def intersectInSegment2DExclusive(a1: VectorConstant, a2: VectorConstant, b1: VectorConstant, b2: VectorConstant): Option[VectorConstant] = {
    intersection2D(a1, a2, b1, b2).filter(p => p.isInSegmentExclusive(a1, a2) && p.isInSegmentExclusive(b1, b2))
  }


  def triangulationPoint2D(p1: VectorConstant, p2: VectorConstant, l1: Double, l2: Double, dir: Boolean): Option[VectorConstant] = {
    val dist = p2 - p1
    val dlen = dist.toDouble
    if (dlen == 0) None
    else {
      val b = (l2 * l2 - l1 * l1 + dlen * dlen) / (2 * dlen)
      val fact = l2 * l2 - b * b
      if (fact < 0) None
      else {
        val h = math.sqrt(fact)
        Some(p2 - (dist.unit * b) + dist.norm2d * (h * (if (dir) -1d else 1d)))
      }
    }
  }

  def midPoint(p1: VectorConstant, p2: VectorConstant) = new VectorConstant((p1.x + p2.x) / 2, (p1.y + p2.y) / 2, (p1.z + p2.z) / 2)

  /**
    * Get Cos Angle between 3 points
    *
    * @param p1 1. point
    * @param p2 2. point
    * @param p3 3. point
    * @return angle between points in cos
    */
  def getAngle(p1: VectorConstant, p2: VectorConstant, p3: VectorConstant): Double = (p2 - p1).cosBetween(p3 - p2)

  def min(a: VectorConstant, b: VectorConstant): VectorConstant = if (a < b) a else b

  def max(a: VectorConstant, b: VectorConstant): VectorConstant = if (a > b) a else b

  /*def isConterClockWise2D(list:Seq[VectorConstant]): Boolean =if(list.size<3)false
    else VectorConstant.pointLocation2D(list(0),list(1),list(2))>0

  def toConterClockWise2D(list:Seq[VectorConstant]): Seq[VectorConstant] =if(isConterClockWise2D(list))list else list.reverse
  def toClockWise2D(list:Seq[VectorConstant]): Seq[VectorConstant] =if(!isConterClockWise2D(list))list else list.reverse*/

  def clipShapeAtLine2D(l1: VectorConstant, l2: VectorConstant, shapePoints: Seq[VectorConstant]): Seq[VectorConstant] = {
    def in(p: VectorConstant) = pointLocation2D(l1, l2, p) >= 0

    val result = ArrayBuffer[VectorConstant]()
    var lastPoint = shapePoints.head
    var lastPointIn = in(lastPoint)
    if (lastPointIn) result += lastPoint
    for (i <- 1 until shapePoints.size) {
      val nextPoint = shapePoints(i)
      if (in(nextPoint)) {
        if (lastPointIn) result += nextPoint
        else {
          for (inter <- intersection2D(l1, l2, lastPoint, nextPoint)) result += inter
          result += nextPoint
        }
      } else { //next point is out
        if (lastPointIn) for (inter <- intersection2D(l1, l2, lastPoint, nextPoint))
          result += inter
      }
      lastPoint = nextPoint
      lastPointIn = in(lastPoint)
    }
    result.toSeq
  }

  def intersectShapes2d(shape: Seq[VectorConstant], cutShape: Seq[VectorConstant]): Seq[VectorConstant] = {
    if (shape.size < 4 || cutShape.size < 4) Seq.empty else {
      var restShape: Seq[VectorConstant] = shape
      for (Seq(a, b) <- cutShape.sliding(2); if restShape.size >= 4) {
        restShape = clipShapeAtLine2D(a, b, restShape)
        if (restShape.nonEmpty && restShape.last != restShape.head) restShape = restShape :+ restShape.head
      }
      restShape
    }
  }

  /** checks, if the Segment(P1,P2) crosses an horzontal Ray starting from Checkpoint going to + x */
  def horRayHit2d(p1: VectorConstant, op1: VectorConstant, op2: VectorConstant): Boolean = {
    //println("Check segment p1:" + p1 + " op1:" + op1 + " " + op2)
    if ((op1.y > p1.y && op2.y > p1.y) || (op1.x < p1.x && op2.x < p1.x ||
      (op1.y < p1.y && op2.y < p1.y) || op1.y == op2.y)) {
      false
    }
    else {
      val dx = 1
      //val p2x = p1.x + 1
      val ody = op2.y - op1.y
      val odx = op2.x - op1.x
      val d = ody * dx
      val ua = (odx * (p1.y - op1.y) - ody * (p1.x - op1.x)) / d
      val result = VectorConstant(p1.x + ua * dx, p1.y, 0)
      //println("HorRayHit chh p1:" + op1 + " p2:" + op2 + " d:" + d + " ua:" + ua + " " + result + " " + result.isInSegment(op1, op2))
      if (ua > 0)
        VectorConstant(p1.x + ua * dx, p1.y, 0).isInSegment(op1, op2)
      else false
    }
  }

  def insideShape(p: VectorConstant, shape: Seq[VectorConstant]): Boolean = if (shape.size < 3) false
  else {
    val sh = if (shape.head != shape.last) shape :+ shape.head else shape
    val hits = sh.sliding(2).count(s => horRayHit2d(p, s.head, s.last))
    println("hits:" + hits)
    (hits % 2) > 0
  }

}


object NULLLINE extends Line3D(NULLVECTOR, NULLVECTOR)

object NULLVECTOR extends VectorConstant(0d, 0d, 0d)

object UNDEFINED_VECTOR extends VectorConstant(java.lang.Double.MIN_VALUE,java.lang.Double.MIN_VALUE,java.lang.Double.MIN_VALUE)
