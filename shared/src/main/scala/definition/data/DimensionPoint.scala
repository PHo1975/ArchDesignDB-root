package definition.data

import java.io.{ ByteArrayInputStream, DataInput, DataInputStream, DataOutput }
import definition.expression.{ BlobConstant, Expression, VectorConstant }
import definition.typ.DataType
import scala.collection.immutable

case class DimensionPoint(var refPoint: VectorConstant, var helpLineLength: Double, var textPos: Option[VectorConstant]) {
  def this(in: DataInput) =  this(Expression.readConstant(in).toVector, in.readDouble,
    if (in.readBoolean) Some(Expression.readConstant(in).toVector) else None)
}


object DimensionPoint {
  def createDimLineList(in: DataInput): IndexedSeq[DimensionPoint] = {
    val code = in.readByte
    if (DataType(code) == DataType.BlobTyp) {
      in.readInt // size
      readList(in)
    } else throw new IllegalArgumentException("Wrong Constant Code when reading Dim Line point list " + code)
  }

  def readList(in: DataInput): immutable.IndexedSeq[DimensionPoint] = {
    val numPoints = in.readInt
    for (_ <- 0 until numPoints) yield new DimensionPoint(in)
  }

  def createDimLineList(blob: BlobConstant): immutable.IndexedSeq[DimensionPoint] = {
    val byteStream = new ByteArrayInputStream(blob.data)
    val inStream = new DataInputStream(byteStream)
    readList(inStream)
  }

  def createBlob(list: Seq[DimensionPoint]): BlobConstant = BlobConstant.fillData(writeList(list, _))

  def writeList(list: Seq[DimensionPoint], out: DataOutput): Unit = {
    out.writeInt(list.size)
    for (d <- list) {
      d.refPoint.write(out)
      out.writeDouble(d.helpLineLength)
      d.textPos match {
        case Some(pos) => out.writeBoolean(true); pos.write(out)
        case None => out.writeBoolean(false)
      }
    }
  }
}