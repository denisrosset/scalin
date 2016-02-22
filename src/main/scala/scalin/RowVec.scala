package scalin

/** Concrete matrix trait. */
trait RowVec[A] extends AbstractRowVec[A] {

  val col: Vec[A]

  def apply(k: Int) = col(k)

  def nextNonZero(k: Int) = col.nextNonZero(k)

  def length = col.length

  override def toString: String = col.toString + ".t"

}

object RowVec {

  def apply[A](v: Vec[A]): RowVec[A] = new RowVec[A] {
    val col = v
  }

}
