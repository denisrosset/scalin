package scalin
package mutable


object dense {

  implicit def vecEngine[A]: VecEngine[A, mutable.DenseVec[A]] = DenseVec.engine[A]

  implicit def matEngine[A]: MatEngine[A, mutable.DenseMat[A]] = DenseMat.engine[A]

}
