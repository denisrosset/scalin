package scalin
package mutable

class UVecEngine[A] extends VecEngine[A, mutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = mutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see immutable.VecEngine as well
    updateFun(res)
    res
  }

}

class UMatEngine[A] extends MatEngine[A, mutable.DenseMat[A]] {

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = mutable.DenseMat.tabulate[A](nRows, nCols)(f)

  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
    val res = new scalin.mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see immutable.VecEngine as well
    updateFun(res)
    res
  }

}

object dense {

  implicit def vecEngine[A]: VecEngine[A, mutable.DenseVec[A]] = new UVecEngine[A]

  implicit def matEngine[A]: MatEngine[A, mutable.DenseMat[A]] = new UMatEngine[A]

}
