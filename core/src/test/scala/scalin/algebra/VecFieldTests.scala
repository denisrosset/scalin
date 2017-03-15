package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Vec

class VecFieldTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]

  test("VecField.pointwiseDiv") {
    val a = vec[Rational](2, 4, 4, 2)
    val b = vec[Rational](2, 1, 4, 1)
    val res = vec[Rational](1, 4, 1, 2)
    engine.pointwiseDiv(a, b) shouldBe res
  }

  test("VecField.div") {
    val a = vec[Rational](2, 4, 4, 6)
    val res = vec[Rational](1, 2, 2, 3)
    engine.div(a, Rational(2)) shouldBe res
  }

}
