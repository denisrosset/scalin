package scalin
package mutable

case class SliceVec[A](vec: mutable.Vec[A], slice: Seq[Int]) extends mutable.AbstractVec[A] with scalin.SliceVec[A] {

  type AsMutable = mutable.Vec[A]
  type AsImmutable = immutable.Vec[A]

  def mutableCopy = this.get[mutable.Vec]
  def toImmutable = this.get[immutable.Vec]

  def update(k: Int, a: A): Unit = vec.update(slice(k), a)

}
