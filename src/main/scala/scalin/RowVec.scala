package scalin

/** Concrete matrix trait. */
trait RowVec[A] extends AbstractRowVec[A] with Generic {

  val col: Vec[A]

  def apply(k: Int) = col(k)

  def nextNonZero(k: Int) = col.nextNonZero(k)

  def length = col.length

  def touch(node: AbstractNode) = col.touch(node) match {
    case Touch.Clean() => Touch.Clean()
    case _ => Touch.Multi()
  }

  def mutableCopy = mutable.RowVec[A](col.mutableCopy)
  def toImmutable = immutable.RowVec[A](col.toImmutable)

  type AsMutable = mutable.RowVec[A]
  type AsImmutable = immutable.RowVec[A]

  override def toString: String = col.toString + ".t"

}

object RowVec {

  def apply[A](v: Vec[A]): RowVec[A] = new RowVec[A] {
    val col = v
  }

  implicit def fromVec[A](v: Vec[A]): RowVec[A] = apply(v)

}
