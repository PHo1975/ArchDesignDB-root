/**
 * Author: Peter Started:05.09.2010
 */
package definition.comm

import java.io.DataInput
import java.io.DataOutput

/** the result of a database command
 * 
 */
case class CommandError(name:String,operationID:Int,reasonID:Int) extends Exception(name) {
	
	def write(out:DataOutput) = {		
		out.writeUTF(if(name==null) "" else name)
		out.writeInt(operationID)
		out.writeInt(reasonID)
	}

}

object CommandError {
	def read(in:DataInput) = {
		val name=in.readUTF()
		val operationID=in.readInt()
		val reasonID=in.readInt
		new CommandError(name,operationID,reasonID)
	}
}