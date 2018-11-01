package scalin.decomposition

import scalin.ScalinSuite
import scalin.immutable.dense._
import spire.math.{Rational, SafeLong}

class LUSuite extends ScalinSuite {
  import scalin.immutable.Mat.rowMajor
    test("Wikipedia example: LU decomposition") {
      val A = rowMajor[Rational](2, 2)(
        4, 3,
        6, 3
      )
      val fact = scalin.decomposition.LU(A)
      A(fact.pivots, ::) shouldBe (fact.L * fact.U)
    }

}

class HermitianSuite extends ScalinSuite {
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
