package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] {

  def touch(node: AbstractNode) = Touch.Clean() // immutable objects cannot be on both sides of :=
}

object Vec extends VecFactory[immutable.Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): immutable.Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

  implicit override def fromAbstractVec[A:Dummy](v: AbstractVec[A]): immutable.Vec[A] = v match {
    case iv: immutable.Vec[A] => iv
    case _ => super.fromAbstractVec[A](v)
  }

}
