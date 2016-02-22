package scalin
package immutable

case class SliceVec[A](vec: immutable.Vec[A], slice: Seq[Int]) extends immutable.Vec[A] with scalin.SliceVec[A] {

  type AsMutable = mutable.Vec[A]
  type AsImmutable = this.type

  def mutableCopy = this.get[mutable.Vec]
  def toImmutable = this

  override def touch(node: AbstractNode) = Touch.Clean() // vec is immutable, so can never be touched from lhs and rhs of :=

}
