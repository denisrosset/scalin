package scalin
package ast

object Slice {

  case class Col[A](mat: AbstractMat[A], c: Int) extends AbstractVec[A] {
    def touch(node: AbstractNode): Touch = mat.touch(node) match {
      case Touch.Clean() => Touch.Clean()
      case Touch.AsIs() => Touch.Col(c)
      case _ => Touch.Multi()
    }
    def apply(r: Int) = mat(r, c)
    def nextNonZero(k: Int) = mat.nextNonZeroInCol(k, c)
    def length = mat.rows
  }

}
