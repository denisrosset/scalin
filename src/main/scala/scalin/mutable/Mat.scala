package scalin
package mutable

import spire.algebra._

trait Mat[A] extends scalin.Mat[A] with Mutable { lhs =>

  def update(r: Int, c: Int, a: A): Unit

  def :=(rhs: AbstractMat[A]): Unit

  def intersectsMutable(mat: AbstractMat[_], rs: Range, cs: Range): Boolean = mat eq lhs

  def intersectsMutable(vec: AbstractVec[_], ks: Range): Boolean = false

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
