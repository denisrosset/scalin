package scalin
package ast

object Slice {

  case class Col[A](mat: AbstractMat[A], c: Int) extends AbstractVec[A] {

    def apply(r: Int) = mat(r, c)
    def nextNonZero(k: Int) = mat.nextNonZeroInCol(k, c)
    def length = mat.rows
    
  }

}
