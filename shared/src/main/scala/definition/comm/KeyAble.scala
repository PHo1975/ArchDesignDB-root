package definition.comm

import definition.data.Referencable

/**
  * Created by Kathi on 29.03.2015.
  */
trait KeyAble[K] extends Referencable {
  def key: K
}
