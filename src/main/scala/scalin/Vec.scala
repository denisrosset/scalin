package scalin

/** Concrete matrix trait. */
trait Vec[A] extends AbstractVec[A] with Generic { self =>

  type AsMutable <: mutable.Vec[A]
  type AsImmutable <: immutable.Vec[A]

  def touch(node: AbstractNode) = if (node eq Vec.this) Touch.AsIs() else Touch.Clean()

  override def toString: String = Printer.vec(Vec.this)

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

}
