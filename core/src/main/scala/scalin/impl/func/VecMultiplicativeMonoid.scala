package scalin
package impl
package func

import spire.algebra.MultiplicativeMonoid

trait VecMultiplicativeMonoid[A, VA <: Vec[A]]
    extends scalin.impl.VecMultiplicativeMonoid[A, VA]
    with scalin.impl.func.VecFactory[A, VA] {

  import spire.syntax.multiplicativeMonoid._

  def kron(lhs: Vec[A], rhs: Vec[A]): VA =
    tabulate(lhs.length * rhs.length) { i =>
      val ri = i % rhs.length
      val li = i / rhs.length
      lhs(li) * rhs(ri)
    }

}
