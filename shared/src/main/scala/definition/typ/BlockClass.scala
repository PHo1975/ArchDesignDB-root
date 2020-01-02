package definition.typ

import java.io.DataInput

import scala.xml.Elem

case class BlockClass(name:String,id:Int,description:String="",blocksize:Int) {
  def toXML:Elem = {
    <BClass name={name} id={id.toString} desc={description} blocksize={blocksize.toString}/>
  }

}

object BlockClass{
  def fromXML(node:scala.xml.Node):BlockClass = {
    val name=(node \"@name").text
    val id=(node \"@id").text.toInt
    val desc=(node \"@desc").text
    val size=(node \"@blocksize").text.toInt
    BlockClass(name,id,desc,size)
  }
  def fromStream(in:DataInput): BlockClass =
    BlockClass(in.readUTF(),in.readInt(),in.readUTF(),in.readInt())
}

case class BlockPropertyFieldDefinition(name:String,blockClass:Int){
  def toXML:Elem = {
    <BlockProperty name={name}  blockclass={blockClass.toString}/>
  }

}

object BlockPropertyFieldDefinition{
  def fromXML (node:scala.xml.Node):BlockPropertyFieldDefinition = {
    val name=(node \"@name").text
    val cl=(node \"@blockclass").text.toInt
    BlockPropertyFieldDefinition(name,cl)
  }
  def fromStream(in:DataInput): BlockPropertyFieldDefinition ={
    BlockPropertyFieldDefinition(in.readUTF(),in.readInt())
  }

}