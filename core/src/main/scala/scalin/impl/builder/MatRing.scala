package scalin
package impl
package builder

trait MatRing[A, MA <: Mat[A]]
    extends scalin.impl.MatRing[A, MA]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, MA] {

  implicit def UMA: scalin.algebra.MatRing[A, UMA]
  implicit def UVA: scalin.algebra.VecRing[A, UVA]

  def determinant(lhs: Mat[A]): A = MahajanVinay[A, UMA](lhs)(UMA)

}
