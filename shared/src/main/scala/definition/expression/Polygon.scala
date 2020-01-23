package definition.expression

import java.awt.geom.Path2D.{Double => Path2dDouble}
import java.awt.geom._
import java.io.{DataInput, DataOutput}

import definition.data.{Referencable, Reference}
import definition.typ.DataType

//case class InterPoint(nx:Double,ny:Double) extends VectorConstant(nx,ny,0)

class Edge(val p1: VectorConstant, val p2: VectorConstant) {
  lazy val minX: Double = scala.math.min(p1.x, p2.x)
  lazy val maxX: Double = scala.math.max(p1.x, p2.x)
  lazy val minY: Double = scala.math.min(p1.y, p2.y)
  lazy val maxY: Double = scala.math.max(p1.y, p2.y)

  def diff: VectorConstant = p2 - p1

  def toLine3D = Line3D(p1, p2 - p1)

  def XYAngle: Double = math.atan2(dy, dx)

  def pointLocation2D(point: VectorConstant): Double = {
    (p2.x - p1.x) * (point.y - p1.y) - (point.x - p1.x) * (p2.y - p1.y)
  }

  def isParallelWith(other: Edge): Boolean = StrictMath.abs(VectorConstant.det2D(other.dx, other.dy, dx, dy)) < Polygon.treshold

  def length: Double = math.sqrt(dx * dx + dy * dy)

  /** intersection with ray
    *
    * @return List of hitpoint if existent, as (positionOnOtherLine,HitPoint)
    *         hitpoint is in other line when positionOnOtherLine >=0 & <=1
    */
  def getIntersectionWith(op1: VectorConstant, op2: VectorConstant): Seq[(Double, VectorConstant)] = {
    val ody = op2.y - op1.y
    val odx = op2.x - op1.x
    val d = ody * dx - odx * dy
    if (d != 0) {
      val ua = (odx * (p1.y - op1.y) - ody * (p1.x - op1.x)) / d
      val ub = (dx * (p1.y - op1.y) - dy * (p1.x - op1.x)) / d
      //println("UA = "+ua)
      if ((ua == 0 && VectorConstant.pointLocation2D(op1, op2, p2) > 0) ||
        (ua == 1 && VectorConstant.pointLocation2D(op1, op2, p1) > 0)) Seq.empty
      else if (ua >= 0 && ua <= 1) {
        val x = p1.x + ua * dx
        val y = p1.y + ua * dy
        List((ub, new VectorConstant(x, y, 0)))
      } else Seq.empty
    }
    else Seq.empty
  }

  def dx: Double = p2.x - p1.x

  def dy: Double = p2.y - p1.y

  def getCutIntersectionWith(op1: VectorConstant, op2: VectorConstant): Seq[(Double, VectorConstant)] = {
    val cutTreshold = 0.000001
    val ody = op2.y - op1.y
    val odx = op2.x - op1.x
    val d = ody * dx - odx * dy
    if (d != 0) {
      val ua = (odx * (p1.y - op1.y) - ody * (p1.x - op1.x)) / d
      val ub = (dx * (p1.y - op1.y) - dy * (p1.x - op1.x)) / d
      if (ua >= -cutTreshold && ua <= 1 + cutTreshold) {
        val x = p1.x + ua * dx
        val y = p1.y + ua * dy
        List((ub, new VectorConstant(x, y, 0)))
      } else Seq.empty
    }
    else Seq.empty
  }

  /** intersection with other edge
    *
    */
  def getIntersectionWith(other: Edge): Option[VectorConstant] =
    if (other.minX > maxX || other.maxX < minX || other.minY > maxY || other.maxY < minY /*||isLinearyDependendFrom(other)*/ ) None
    else {
      val d = other.dy * dx - other.dx * dy
      if (d == 0) None // parallel
      else {
        val ua = (other.dx * (p1.y - other.p1.y) - other.dy * (p1.x - other.p1.x)) / d
        val ub = (dx * (p1.y - other.p1.y) - dy * (p1.x - other.p1.x)) / d
        if ((ua == 0 && (ub == 0 || ub == 1)) || (ua == 1 && (ub == 0 || ub == 1))) None // both are endpoints
        else if (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1) {
          // inside the segments
          val x = p1.x + ua * dx
          val y = p1.y + ua * dy
          Some(new VectorConstant(x, y, 0))
        }
        else None
      }
    }

  override def toString: String = "Edge " + p1 + " - " + p2 + " "

}


case class PointList(points: Seq[VectorConstant]) {

  lazy val path2D: Path2D.Double = createPath2D
  lazy val edges: Seq[Edge] = points.indices.map(i => new Edge(points(i), points(nextVect(i))))
  lazy val minX: Double = if (points.isEmpty) Double.MaxValue else points.reduceLeft((a, b) => if (a.x < b.x) a else b).x
  lazy val minY: Double = if (points.isEmpty) Double.MaxValue else points.reduceLeft((a, b) => if (a.y < b.y) a else b).y
  lazy val maxX: Double = if (points.isEmpty) Double.MinValue else points.reduceLeft((a, b) => if (a.x > b.x) a else b).x
  lazy val maxY: Double = if (points.isEmpty) Double.MinValue else points.reduceLeft((a, b) => if (a.y > b.y) a else b).y
  lazy val getArea: Double = {
    if (numVecs < 3) 0
    else (points.head.x * (points(1).y - points(numVecs - 1).y) + points(numVecs - 1).x * (points.head.y - points(numVecs - 2).y) +
      (1 until numVecs - 1).foldLeft(0d)((r, ix) => r + points(ix).x * (points(ix + 1).y - points(ix - 1).y))) / 2
  }
  lazy val isClockWise: Boolean = getArea < 0
  lazy val centerPoint: VectorConstant = if (points.isEmpty) NULLVECTOR else
                                                                          points.foldLeft[VectorConstant](NULLVECTOR)(_ + _) * (1 / points.size)

  def createPath2D: Path2dDouble = {
    val path = new Path2dDouble
    if (points.size > 2) {
      path.moveTo(points.head.x, points.head.y)
      for (i <- 1 until points.size; p = points(i))
        path.lineTo(p.x, p.y)
      path.closePath()
    }
    path
  }

  def getPrevPoint(point: Int): Int = if (point - 1 >= 0) point - 1 else points.size - 1

  def getMidPoint: VectorConstant = if (numVecs < 3) mid else {
    val area = getArea
    val sp = points(numVecs - 1)
    val snp = points.head
    val sfact = sp.x * snp.y - snp.x * sp.y
    var rx = (sp.x + snp.x) * sfact
    var ry = (sp.y + snp.y) * sfact
    for (i <- 0 until numVecs - 1) {
      val p = points(i)
      val np = points(i + 1)
      val fact = p.x * np.y - np.x * p.y
      rx += (p.x + np.x) * fact
      ry += (p.y + np.y) * fact
    }
    val fact2 = 6d * area
    new VectorConstant(rx / fact2, ry / fact2, 0)

  }

  private def mid = if (points.isEmpty) NULLVECTOR else new VectorConstant(points.foldLeft(0d)(_ + _.x) / points.size,
    points.foldLeft(0d)(_ + _.y) / points.size, points.foldLeft(0d)(_ + _.z) / points.size)

  def getUmfang: Double = if (numVecs < 3) 0d else
                                                (1 until numVecs).foldLeft(0d)((s, ix) => s + (points(ix) - points(ix - 1)).toDouble) + (points.head - points(numVecs - 1)).toDouble

  def minEdgeDistanceToPoint(point: VectorConstant): Double = (edges map (e => scala.math.abs(e.pointLocation2D(point)))).min(Ordering.Double.TotalOrdering)

  def translate(v: VectorConstant): PointList = PointList(points.map(_ + v))

  def translatePoints(hitPoints: Set[VectorConstant], delta: VectorConstant): PointList = PointList(points.map(p => if (hitPoints.contains(p)) p + delta else p))

  def transform(trans: VectorConstant => VectorConstant): PointList = PointList(points.map(trans))

  def pointsOutsideFromEdge(edge: Edge): Boolean = {
    points.exists(edge.pointLocation2D(_) > 0)
  }

  def clockWise: PointList = if (getArea > 0) reverse else this

  def reverse: PointList = PointList(points.reverse)

  override def toString: String = "P(" + points.mkString(";") + ")"

  def write(file: DataOutput): Unit = {
    file.writeInt(points.size)
    points.foreach(_.write(file))
  }

  def removeStraightEdges(): PointList = {
    val straightEdges = angles.zip(points.indices.iterator).filter { case (angle, _) => StrictMath.abs(StrictMath.abs(angle) - 1d) < Polygon.treshold }.toSeq
    if (straightEdges.isEmpty) this
    else PointList( points.zipWithIndex.filterNot {
      case (_, pix) => straightEdges.exists { case (_, aix) => aix == pix }
    }.map(_._1))
  }

  def angles: Iterator[Double] = Polygon.ringLoop(points).sliding(3).map { case Seq(p1, p2, p3) =>
    VectorConstant.getAngle(p1, p2, p3)
  }

  def removeDoublePoints(): PointList = {
    val doublePoints = points.indices.filter(i => VectorConstant.similar(points(i), points(nextVect(i))))
    if (doublePoints.isEmpty) this
    else PointList(points.zipWithIndex.filterNot { case (_, pix) => doublePoints.contains(pix) }.map(_._1))
  }

  def encode(): String = points.map(_.encode).mkString("§")

  def cosAngles: Iterator[Double] = Polygon.ringLoop(points).sliding(3).map { case Seq(p1, p2, p3) =>
    StrictMath.acos(VectorConstant.getAngle(p1, p2, p3)) - (if (VectorConstant.pointLocation2D(p1, p2, p3) < 0) StrictMath.PI else 0)
  }

  def edgeLengths: Iterator[Double] = points.indices.iterator.map(ix => (points(nextVect(ix)) - points(ix)).toDouble)

  def nextVect(i: Int): Int = if (i < numVecs - 1) i + 1 else 0

  def numVecs: Int = points.size
}


class Polygon(val parents: Seq[Referencable], val pathList: Seq[PointList] = Seq.empty) extends Constant {

  lazy val minX: Double = if (pathList.isEmpty) Double.MaxValue else pathList.reduceLeft((a, b) => if (a.minX < b.minX) a else b).minX
  lazy val minY: Double = if (pathList.isEmpty) Double.MaxValue else pathList.reduceLeft((a, b) => if (a.minY < b.minY) a else b).minY
  lazy val maxX: Double = if (pathList.isEmpty) Double.MinValue else pathList.reduceLeft((a, b) => if (a.maxX > b.maxX) a else b).maxX
  lazy val maxY: Double = if (pathList.isEmpty) Double.MinValue else pathList.reduceLeft((a, b) => if (a.maxY > b.maxY) a else b).maxY
  lazy val getAreaValue: Double = -1d * pathList.foldLeft(0d)((sum, value) => {sum + value.getArea})
  lazy val getUmfangValue:Double = pathList.foldLeft(0d)((sum,value)=>{sum+value.getUmfang})
  lazy val path: Path2dDouble = toPath
  lazy val area = new Area(path)
  private lazy val testPoint = new Point2D.Double

  def toInt: Int = toDouble.toInt

  def toLong: Long = toDouble.toLong

  def toDouble: Double = pathList.size

  def toBoolean: Boolean = pathList.nonEmpty

  def getNative: Any = this

  def getType: DataType.Value = DataType.PolygonTyp

  override def toString: String = getTerm

  import Polygon.{areaTreshold, contTreshold, treshold}

  //def createCopy(): Expression = { new Polygon(parents,pathList) }
  def getTerm: String = "Pa[" + pathList.mkString("|") + "]"

  def getBounds = new Rectangle2D.Float(minX.toFloat, minY.toFloat, (maxX - minX).toFloat, (maxY - minY).toFloat)

  def isEmpty: Boolean = scala.math.abs(getAreaValue) < areaTreshold

  def toPath: Path2D.Double = {
    val pa = new Path2D.Double
    if (pathList.nonEmpty) {
      for (pList <- pathList; if pList.points.size > 2) {
        pa.moveTo(pList.points.head.x, pList.points.head.y)
        for (ix <- 1 until pList.points.size)
          pa.lineTo(pList.points(ix).x, pList.points(ix).y)
        pa.closePath()
      }
    }
    pa
  }

  def toPathTransformed(trans: VectorConstant => VectorConstant): Path2D.Double = {
    val pa = new Path2D.Double
    if (pathList.nonEmpty) {
      for (pList <- pathList; if pList.points.size > 2) {
        val transf = trans(pList.points.head)
        pa.moveTo(transf.x, transf.y)
        for (ix <- 1 until pList.points.size; p = trans(pList.points(ix)))
          pa.lineTo(p.x, p.y)
        pa.closePath()
      }
    }
    pa
  }

  def toLinePathTransformed(trans: VectorConstant => VectorConstant): Path2D.Double = {
    val pa = new Path2D.Double
    for (lh <- pathList.headOption; ph <- lh.points.headOption; fp = trans(ph)) {
      pa.moveTo(fp.x, fp.y)
      for (ix <- 1 until lh.points.size; p = trans(lh.points(ix)))
        pa.lineTo(p.x, p.y)
    }
    pa
  }

  def transform(trans: VectorConstant => VectorConstant) = new Polygon(parents, pathList.map(_.transform(trans)))

  /** finds the minimal and maximal distances to the point (0,0) of this polygon
    *
    */
  def findMinMaxHatchDistances(dir: VectorConstant, norm: VectorConstant, startPoint: VectorConstant): (Double, Double) = {
    val du = dir.unit
    val dir2 = du * du
    //val norm=new VectorConstant(-du.y,du.x,0)
    var minDist = Double.MaxValue
    var maxDist = Double.MinValue
    for (pl <- pathList; p <- pl.points) {
      val hdist = p.hatchDistance(du, dir2, norm, startPoint)
      if (hdist < minDist) minDist = hdist
      if (hdist > maxDist) maxDist = hdist
    }
    (minDist, maxDist)
  }

  def write(file: DataOutput): Unit = {
    //System.out.println("write poly ")
    file.writeByte(DataType.PolygonTyp.id)
    file.writeInt(parents.size)
    parents.foreach(_.ref.write(file))
    val numPaths = pathList.foldLeft(0)((n, path) => if (path.points.size > 1) n + 1 else n)
    file.writeInt(numPaths)
    pathList.filter(_.points.size > 1).foreach(_.write(file))
  }

  def encode: String = "$A[" + pathList.map(_.encode()).mkString("|") + "]"

  override def toPolygon: Polygon = this

  def translate(v: VectorConstant) = new Polygon(parents, pathList.map(_.translate(v)))

  /** translates only points of this polygon that are part of the given points Sequence
    *
    */
  def translatePoints(points: Set[VectorConstant], delta: VectorConstant) = new Polygon(parents, pathList.map(_.translatePoints(points, delta)))

  def intersectionsWith(p1: VectorConstant, p2: VectorConstant): Seq[(Double, VectorConstant)] =
    pathList.flatMap(_.edges.flatMap(_.getIntersectionWith(p1, p2))).sortBy(a => a._1)(Ordering.Double.TotalOrdering) //.distinct

  def contains(v: VectorConstant): Boolean = {
    testPoint.x = v.x
    testPoint.y = v.y
    if (area.contains(testPoint)) {
      val med = minEdgeDistanceToPoint(v)
      //println("check contains "+med)
      med > contTreshold
    } else false
  }

  def minEdgeDistanceToPoint(point: VectorConstant): Double = (pathList map (_.minEdgeDistanceToPoint(point))).min(Ordering.Double.TotalOrdering)

  def intersectsWith(other: Polygon): Boolean = {
    if (other.maxX - treshold <= minX || other.minX + treshold >= maxX || other.maxY - treshold <= minY || other.minY + treshold >= maxY) false
    else {
      scala.math.abs(intersect(other).getAreaValue) > treshold
    }
    //else other.pathList.exists(_.points.exists(contains(_)))|| pathList.exists(_.points.exists(other.contains(_)))
  }

  def intersect(other: Polygon): Polygon = {
    val cl = areaClone
    cl.intersect(other.area)
    new Polygon(parents ++ other.parents, Polygon.areaToPoints(cl))
    //setArea(cl)
  }

  def subtract(other: Polygon): Polygon = {
    val cl = areaClone
    cl.subtract(other.area)
    //setArea(cl)
    new Polygon(parents ++ other.parents, Polygon.areaToPoints(cl))
  }

  def areaClone: Area = area.clone.asInstanceOf[Area]

  def add(other: Polygon): Polygon = {
    val cl = areaClone
    cl.add(other.area)
    //setArea(cl)
    new Polygon(parents ++ other.parents, Polygon.areaToPoints(cl))
  }

  def setArea(newArea: Area): Polygon = new Polygon(parents, Polygon.areaToPoints(newArea))

  def iterateAllPoints:Iterator[VectorConstant]= if(pathList.isEmpty) Nil.iterator else
    new Iterator[VectorConstant] {
      var currentPointList=0
      var currentIterator:Iterator[VectorConstant]=pathList.head.points.iterator
      override def hasNext: Boolean = if (currentIterator.hasNext) true
      else {
        currentPointList+=1
        if(currentPointList < pathList.size ) {
          currentIterator=pathList(currentPointList).points.iterator
          currentIterator.hasNext
        } else false
      }

      override def next(): VectorConstant = currentIterator.next
    }

}


object Polygon {
  val treshold = 0.000001d
  val contTreshold = 0.00000000000001d
  val areaTreshold = 0.0000001d

  def decode(text: String): (Polygon, Int) = {
    val end = text.indexOf(']', 3)
    val parts = text.substring(3, end).split('|')
    (new Polygon(Seq.empty, parts.map(decodePointList).toIndexedSeq), end + 1)
  }

  def decodePointList(text: String): PointList = PointList(text.split('§').view.map(VectorConstant.decode(_)._1).toIndexedSeq)

  def areaToPoints(area: Area): Seq[PointList] = {
    val iter = area.getPathIterator(null)
    //print("wind :"+iter.getWindingRule()==PathIterator.WIND_NON_ZERO)
    val retArray = new Array[Double](6)
    val pathList = new collection.mutable.ArrayBuffer[PointList]()
    var pList: collection.mutable.ArrayBuffer[VectorConstant] = null
    while (!iter.isDone) {
      iter.currentSegment(retArray) match {
        case PathIterator.SEG_MOVETO => pList = collection.mutable.ArrayBuffer[VectorConstant](new VectorConstant(retArray(0), retArray(1), 0))
        case PathIterator.SEG_LINETO => pList += new VectorConstant(retArray(0), retArray(1), 0)
        case PathIterator.SEG_CLOSE => pathList += PointList(pList.toSeq)
      }
      iter.next()
    }
    pathList.toSeq
  }

  def ringLoop[A](s: Iterable[A]): Iterator[A] = {
    new Iterator[A] {
      val intIter: Iterator[A] = s.iterator
      var state = 0

      def hasNext: Boolean = state match {
        case 0 => s.nonEmpty
        case 1 => true
        case _ => false
      }

      def next(): A = state match {
        case 0 => state = 1; s.last
        case 1 => if (intIter.hasNext) intIter.next() else {
          state = 2
          s.head
        }
        case _ => throw new IllegalArgumentException("Read after end of Ring")
      }
    }
  }

  def midOfPointList(pointLists: Seq[PointList]): VectorConstant =
    if (pointLists.isEmpty) NULLVECTOR
    else {
      val sum = pointLists.foldLeft((0d, 0d, 0d, 0d))((i, v) => {
        val area = -1d * v.getArea
        val vm = v.getMidPoint
        (i._1 + vm.x * area, i._2 + vm.y * area, i._3 + vm.z * area, i._4 + area)
      })
      val size = sum._4
      new VectorConstant(sum._1 / size, sum._2 / size, sum._3 / size)
    }

  private[expression] def apply(file: DataInput) = {
    new Polygon(for (_ <- 0 until file.readInt) yield Reference(file), for (_ <- 0 until file.readInt) yield readPointList(file))
  }

  def readPointList(file: DataInput) = PointList(for (_ <- 0 until file.readInt) yield {
    file.readByte
    new VectorConstant(file.readDouble, file.readDouble, file.readDouble)
  })


}


object NULLPOLYGON extends Polygon(Seq.empty)