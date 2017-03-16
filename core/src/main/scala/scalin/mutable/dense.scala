package scalin
package mutable

object dense {

  implicit def vecEngine[A]: scalin.VecEngine[A, mutable.DenseVec[A]] = DenseVec.engine[A]

  implicit def matEngine[A]: scalin.MatEngine[A, mutable.DenseMat[A]] = DenseMat.engine[A]

}
