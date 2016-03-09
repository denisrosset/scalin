package scalin
package impl
package func

import spire.syntax.multiplicativeMonoid._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.impl.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.func.MatEngine[A, MA] {

  //// With `MultiplicativeMonoid[A]`, returning matrix

  def kron(lhs: Mat[A], rhs: Mat[A]): MA =
    tabulate(lhs.rows * rhs.rows, lhs.cols * rhs.cols) { (r, c) =>
      val rr = r % rhs.rows
      val rl = r / rhs.rows
      val cr = c % rhs.cols
      val cl = c / rhs.cols
      lhs(rl, cl) * rhs(rr, cr)
    }

}
