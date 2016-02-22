package scalin
package mutable

import spire.algebra._

trait Vec[A] extends scalin.Vec[A] with mutable.AbstractVec[A] with Mutable { lhs =>

  override def apply(indices: Seq[Int]): mutable.SliceVec[A] = mutable.SliceVec(lhs, indices)

  override def apply(mask: scalin.AbstractVec[Boolean]): mutable.SliceVec[A] = apply(Util.maskToIndices(mask))

  def intersectsMutable(mat: AbstractMat[_], rs: Range, cs: Range): Boolean = false

  def intersectsMutable(vec: AbstractVec[_], ks: Range): Boolean = vec eq lhs

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    DenseVec.tabulate[A](length)(f)

}
