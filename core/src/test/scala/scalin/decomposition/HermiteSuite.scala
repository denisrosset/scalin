package scalin.decomposition

import scalin.ScalinSuite
import scalin.immutable.dense._
import spire.algebra.{Eq, EuclideanRing}
import spire.math.{Rational, SafeLong}

class HermiteSuite extends ScalinSuite {
  // Euclidean division that respects nonnegativity of the remainder
  implicit val safeLongEuclideanRing: EuclideanRing[SafeLong] = new EuclideanRing[SafeLong] {
    override def minus(a:SafeLong, b:SafeLong): SafeLong = a - b
    def negate(a:SafeLong): SafeLong = -a
    val one: SafeLong = SafeLong.one
    def plus(a:SafeLong, b:SafeLong): SafeLong = a + b
    override def pow(a:SafeLong, b:Int): SafeLong = a pow b
    override def times(a:SafeLong, b:SafeLong): SafeLong = a * b
    val zero: SafeLong = SafeLong.zero
    override def fromInt(n: Int): SafeLong = SafeLong(n)
    def lcm(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a lcm b
    def gcd(a:SafeLong, b:SafeLong)(implicit ev: Eq[SafeLong]): SafeLong = a gcd b
    def euclideanFunction(a:SafeLong): BigInt = a.abs.toBigInt
    override def equotmod(a: SafeLong, b: SafeLong): (SafeLong, SafeLong) = {
    val (qt, rt) = a /% b // truncated quotient and remainder
      if (rt.signum >= 0) (qt, rt)
      else if (b.signum > 0) (qt - 1, rt + b)
      else (qt + 1, rt - b)
    }
    def equot(a: SafeLong, b: SafeLong): SafeLong = {
      val (qt, rt) = a /% b // truncated quotient and remainder
      if (rt.signum >= 0) qt
      else if (b.signum > 0) qt - 1
      else qt + 1
    }
    def emod(a: SafeLong, b: SafeLong): SafeLong = {
      val rt = a % b // truncated remainder
      if (rt.signum >= 0) rt
      else if (b > 0) rt + b
      else rt - b
    }
  }
  import scalin.immutable.Mat.rowMajor
  // examples transposed because we implement the column version
  test("Wikipedia example: 1st example") {
    val A = rowMajor[SafeLong](4, 4)(
      3, 0, 0, 0,
      3, 1, 0, 0,
      1, 0, 19, 0,
      4, 0, 16, 3
    )
    val H = rowMajor[SafeLong](4, 4)(
      3, 0, 0, 0,
      0, 1, 0, 0,
      1, 0, 19, 0,
      1, 0, 1, 3
    )
    val U = rowMajor[SafeLong](4, 4)(
      1, 0, 0, 0,
      -3, 1, 0, 0,
      0, 0, 1, 0,
      -1, 0, -5, 1
    )
    val D = scalin.decomposition.Hermite(A)
    D.H shouldBe H
    D.U shouldBe U
  }
  test("Wikipedia example: 3rd example") {
    val A = rowMajor[SafeLong](4,3)(
      2,5,8,
      3,6,3,
      6,1,1,
      2,6,1)
    val H = rowMajor[SafeLong](4, 3)(
      1, 0, 0,
      0, 3, 0,
      50, 28, 61,
      -11, -2, -13
    )
    val U = rowMajor[SafeLong](3, 3)(
      9, 5, 11,
      -5, -2, -6,
      1, 0, 1
    )
    val D = scalin.decomposition.Hermite(A)
    D.H shouldBe H
    D.U shouldBe U
  }

}
