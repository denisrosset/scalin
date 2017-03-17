package scalin
package immutable

object dense {

  implicit def vecEngine[A]: scalin.VecEngine[A, immutable.DenseVec[A]] = DenseVec.engine[A]

  implicit def matEngine[A]: scalin.MatEngine[A, immutable.DenseMat[A]] = DenseMat.engine[A]

}
