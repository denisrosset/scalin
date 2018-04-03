package scalin
package immutable

object csc {

  implicit def vecEngine[A]: scalin.VecEngine[A, immutable.DenseVec[A]] = DenseVec.defaultEngine[A]

  implicit def matEngine[A:Sparse]: scalin.MatEngine[A, immutable.CSCMat[A]] = CSCMat.defaultEngine[A]

}
