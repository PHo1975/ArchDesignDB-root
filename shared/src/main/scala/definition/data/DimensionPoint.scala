package definition.data
import definition.expression.VectorConstant
import java.io.DataInput
import definition.typ.DataType
import definition.expression.BlobConstant
import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.io.DataOutput
import definition.expression.Expression

case class DimensionPoint(var refPoint:VectorConstant,var helpLineLength:Double,var textPos:Option[VectorConstant]){
  def this(in:DataInput) = {    
    this(Expression.readConstant(in).toVector,in.readDouble,if(in.readBoolean)Some(Expression.readConstant(in).toVector) else None)
  }
  //if(helpLineLength>0 ) println("DimensionPoint "+toString)
}

object DimensionPoint {
  def createDimLineList(in:DataInput):IndexedSeq[DimensionPoint]= {
	  val code=in.readByte	  
	  if(DataType(code)==DataType.BlobTyp) {
	    in.readInt // size
	    readList(in)
	  } else throw new IllegalArgumentException("Wrong Constant Code when reading Dim Line point list "+code)
	}
  
  def readList(in:DataInput)={
    val numPoints=in.readInt	    
	  for(i<-0 until numPoints) yield new DimensionPoint(in)
  } 
  
  def createDimLineList(blob:BlobConstant)= {
    val byteStream=new ByteArrayInputStream(blob.data) 
  	val inStream=new DataInputStream(byteStream)
    readList(inStream)
  }
  
  def writeList(list:Seq[DimensionPoint],out:DataOutput)= {
    out.writeInt(list.size)
    for(d<-list) {
       d.refPoint.write(out)
       out.writeDouble(d.helpLineLength)
       d.textPos match {
         case Some(pos)=> out.writeBoolean(true);pos.write(out)
         case None=> out.writeBoolean(false)
       }
    }  
  }
  
  def createBlob(list:Seq[DimensionPoint])= BlobConstant.fillData( writeList(list,_) )
}