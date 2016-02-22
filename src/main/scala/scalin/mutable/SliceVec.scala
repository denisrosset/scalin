package scalin
package mutable

case class SliceVec[A](vec: mutable.Vec[A], slice: Seq[Int]) extends mutable.AbstractVec[A] with scalin.SliceVec[A] {

  def update(k: Int, a: A): Unit = vec.update(slice(k), a)

}
