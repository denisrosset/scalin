package scalin
package immutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.algebra.VecEngine[A, immutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = immutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
    updateFun(res)
    res.result()
  }

}

class MatEngine[A] extends scalin.algebra.MatEngine[A, immutable.DenseMat[A]] {

  type UVA = mutable.DenseVec[A]
  def UVA = mutable.dense.vecEngine[A]

  type UMA = mutable.DenseMat[A]
  def UMA = mutable.dense.matEngine[A]

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = immutable.DenseMat.tabulate[A](nRows, nCols)(f)

  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
    val res = new mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols))
    // TODO: zero semantics
    updateFun(res)
    res.result()
  }

  def alloc(rows: Int, cols: Int) = new mutable.DenseMat(rows, cols, new Array[AnyRef](rows * cols))

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(mat: UMA) = mat.result()

}


object dense {

  implicit def vecEngine[A]: scalin.algebra.VecEngine[A, immutable.DenseVec[A]] = new VecEngine[A]

  implicit def matEngine[A]: scalin.algebra.MatEngine[A, immutable.DenseMat[A]] = new MatEngine[A]

}
