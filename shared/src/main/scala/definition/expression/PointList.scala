package definition.expression

import java.awt.geom.Path2D.{Double => Path2dDouble}
import java.io.DataOutput

object EmptyPointList extends PointList(Seq.empty)

case class PointList(points: Seq[VectorConstant]) {

  lazy val path2D: Path2dDouble = createPath2D
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

  def conterClockWise: PointList = if (getArea > 0) this else reverse

  def reverse: PointList = PointList(points.reverse)

  override def toString: String = "P(" + points.mkString(";") + ")"

  def write(file: DataOutput): Unit = {
    file.writeInt(points.size)
    points.foreach(_.write(file))
  }

  def removeStraightEdges(): PointList = {
    val straightEdges = angles.zip(points.indices.iterator).filter { case (angle, _) => Math.abs(Math.abs(angle) - 1d) < Polygon.treshold }.toSeq
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

  def rotatePoints(by:Int): Iterator[VectorConstant] ={
    val(a,b)=points.iterator.splitAt(by % numVecs)
    b++a
  }

  def isSameAs(other:PointList): Boolean = if(numVecs==other.numVecs){
    points.indices.exists(i=> !rotatePoints(i).zip(other.points).exists{case(a,b)=>a!=b})

  } else false

  def encode(): String = points.map(_.encode).mkString("§")

  def cosAngles: Iterator[Double] = Polygon.ringLoop(points).sliding(3).map { case Seq(p1, p2, p3) =>
    Math.acos(VectorConstant.getAngle(p1, p2, p3)) - (if (VectorConstant.pointLocation2D(p1, p2, p3) < 0) Math.PI else 0)
  }

  def edgeLengths: Iterator[Double] = points.indices.iterator.map(ix => (points(nextVect(ix)) - points(ix)).toDouble)

  def nextVect(i: Int): Int = if (i < numVecs - 1) i + 1 else 0

  def numVecs: Int = points.size
}
