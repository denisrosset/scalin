package scalin

/** Concrete matrix trait. */
trait Vec[A] extends AbstractVec[A] { self =>

  override def toString: String = Printer.vec(Vec.this)

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

}
