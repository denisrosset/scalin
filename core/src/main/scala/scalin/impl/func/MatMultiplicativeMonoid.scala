package scalin
package impl
package func

import spire.syntax.multiplicativeMonoid._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.impl.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.func.MatEngine[A, MA] {

  //// With `MultiplicativeMonoid[A]`, returning matrix

  def kron(lhs: Mat[A], rhs: Mat[A]): MA =
    tabulate(lhs.nRows * rhs.nRows, lhs.nCols * rhs.nCols) { (r, c) =>
      val rr = r % rhs.nRows
      val rl = r / rhs.nRows
      val cr = c % rhs.nCols
      val cl = c / rhs.nCols
      lhs(rl, cl) * rhs(rr, cr)
    }

}
