package scalin

/** Concrete matrix trait. */
trait RowVec[A, +V <: Vec[A]] extends AbstractRowVec[A] with Generic {

  val t: V

  def apply(k: Int) = t(k)

  def length = t.length

  def touch(node: AbstractNode) = t.touch(node) match {
    case Touch.Clean() => Touch.Clean()
    case _ => Touch.Multi()
  }

  def mutableCopy = mutable.RowVec[A, t.AsMutable](t.mutableCopy)
  def toImmutable = immutable.RowVec[A, t.AsImmutable](t.toImmutable)

  type AsMutable = mutable.RowVec[A, t.AsMutable]
  type AsImmutable = immutable.RowVec[A, t.AsImmutable]

//  override def toString: String = Printer.vec(Vec.this)

}

object RowVec {

  def apply[A, V <: Vec[A]](v: V): RowVec[A, V] = new RowVec[A, V] {
    val t = v
  }

}

trait RowVecWrap[V[A] <: Vec[A], R[A] <: RowVec[A, V[A]]] {
  def wrap[A](v: V[A]): R[A]
}

abstract class RowVecWrap0 {

  object generic extends RowVecWrap[scalin.Vec, ({type L[A] = scalin.RowVec[A, scalin.Vec[A]]})#L] {
    def wrap[A](v: Vec[A]): scalin.RowVec[A, scalin.Vec[A]] = scalin.RowVec[A, scalin.Vec[A]](v)
  }

  implicit def generic[V[A] <: Vec[A]]: RowVecWrap[V, ({type L[A] = scalin.RowVec[A, V[A]]})#L] =
    generic.asInstanceOf[RowVecWrap[V, ({type L[A] = scalin.RowVec[A, V[A]]})#L]]

}

object RowVecWrap extends RowVecWrap0 {

  /*
  implicit def mutable[V[A] <: mutable.Vec[A]]: RowVecWrap[V, mutable.RowVec[A, V[A]]] =
    new RowVecWrap[V, mutable.RowVec[A, V[A]]] {
      def wrap[A](v: V[A]): mutable.RowVec[A, V[A]] = mutable.RowVec(v)
    }
 */
}

/*
object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

}
 */
