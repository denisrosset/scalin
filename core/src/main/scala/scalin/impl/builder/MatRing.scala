package scalin
package impl
package builder

trait MatRing[A, MA <: Mat[A], UA <: mutable.Mat[A]]
    extends scalin.impl.MatRing[A, MA]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, MA, UA] {

  implicit def UA: scalin.algebra.MatRing[A, UA]

  def determinant(lhs: Mat[A]): A = MahajanVinay[A, UA](lhs)(UA)

}
