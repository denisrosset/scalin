package scalin
package impl
package func

trait MatRing[A, MA <: Mat[A]]
    extends scalin.impl.MatRing[A, MA]
    with scalin.impl.func.MatMultiplicativeMonoid[A, MA] {

  def determinant(lhs: Mat[A]): A = ??? // TODO: call mutable implementation

}
