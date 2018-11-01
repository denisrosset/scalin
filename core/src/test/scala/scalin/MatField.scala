package scalin

import scalin.decomposition.LU
import spire.math.Rational

class MatField extends ScalinSuite {

  import scalin.immutable.dense._
  import spire.laws.arb.rational

  test("Inverses of matrices of determinant one") {
    forAll(Mats.genDetOne[Rational](4)) { m =>
      (m * m.inverse) shouldBe Mat.eye[Rational](4)
    }
  }

  test("Inverses of full rank matrices") {
    forAll(Mats.genFullRank[Rational](4)) { m =>
      (m * m.inverse) shouldBe Mat.eye[Rational](4)
    }
  }

  test("LU decomposition") {
    forAll(Mats.genFullRank[Rational](3)) { m =>
      import scalin.mutable.dense._
      val dec: LU[Rational] = LU.inPlaceLU(implicitly[scalin.mutable.MatEngine[Rational]].fromMat(m))
      val lu = dec.L * dec.U
      cforRange(0 until m.nRows) { k => m(dec.pivot(k), ::) shouldBe lu(k, ::) }
    }
  }

}
