package scalin
package immutable

object dense {

  implicit def vecEngine[A]: VecEngine[A, immutable.DenseVec[A]] = DenseVec.engine[A]

  implicit def matEngine[A]: MatEngine[A, immutable.DenseMat[A]] = DenseMat.engine[A]

}
