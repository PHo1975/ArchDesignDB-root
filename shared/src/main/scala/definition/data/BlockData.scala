package definition.data

import java.io.DataOutput

import definition.comm.KeyAble

class BlockData(val ref:Reference, val data:Array[Byte]) extends Referencable with KeyAble[Reference] {
  override def key: Reference =  ref

  override def write(d: DataOutput): Unit= d.write(data)

}

