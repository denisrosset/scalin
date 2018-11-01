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
