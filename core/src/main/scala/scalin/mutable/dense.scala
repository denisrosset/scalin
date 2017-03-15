package scalin
package mutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.algebra.VecEngine[A, mutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = mutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see immutable.VecEngine as well
    updateFun(res)
    res
  }

}

class MatEngine[A](implicit val UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]])
    extends scalin.algebra.MatEngine[A, mutable.DenseMat[A]] {

  type UMA = mutable.DenseMat[A]
  def UMA = this

  type UVA = mutable.DenseVec[A]

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = mutable.DenseMat.tabulate[A](nRows, nCols)(f)

  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
    val res = new scalin.mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see immutable.VecEngine as well
    updateFun(res)
    res
  }

  def alloc(nRows: Int, nCols: Int) = new mutable.DenseMat(nRows, nCols, new Array[AnyRef](nRows * nCols))

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(mat: UMA) = mat

}

object dense {

  implicit def vecEngine[A]: scalin.algebra.VecEngine[A, mutable.DenseVec[A]] =
    new VecEngine[A]

  implicit def matEngine[A]: scalin.algebra.MatEngine[A, mutable.DenseMat[A]] =
    new MatEngine[A]

}
