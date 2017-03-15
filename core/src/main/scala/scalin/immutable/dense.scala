package scalin
package immutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

class IVecEngine[A] extends scalin.VecEngine[A, immutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = immutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
    updateFun(res)
    res.result()
  }

}

class IMatEngine[A] extends scalin.MatEngine[A, immutable.DenseMat[A]] {

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = immutable.DenseMat.tabulate[A](nRows, nCols)(f)

  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
    val res = new mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols))
    // TODO: zero semantics
    updateFun(res)
    res.result()
  }

}


object dense {

  implicit def vecEngine[A]: VecEngine[A, immutable.DenseVec[A]] = new IVecEngine[A]

  implicit def matEngine[A]: MatEngine[A, immutable.DenseMat[A]] = new IMatEngine[A]

}
