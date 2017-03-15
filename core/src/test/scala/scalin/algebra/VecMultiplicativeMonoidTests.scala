package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Vec

class VecMultiplicativeMonoidTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]

  test("VecMultiplicativeMonoid.ones") {
    engine.ones(3) shouldBe vec[Rational](1, 1, 1)
  }

  test("VecMultiplicativeMonoid.product") {
    engine.product(vec[Rational](1, 2, 3, 4)) shouldBe Rational(24)
  }

  test("VecMultiplicativeMonoid.times") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.times(a, Rational(2)) shouldBe vec[Rational](2, 4, 6, 8)
    engine.times(Rational(2), a) shouldBe vec[Rational](2, 4, 6, 8)
    // TODO: test with a noncommutative ring
  }

  test("VecMultiplicativeMonoid.pointwiseTimes") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.pointwiseTimes(a, a) shouldBe vec[Rational](1, 4, 9, 16)
  }

  test("VecMultiplicativeMonoid.kron") {
    val a = vec[Rational](1, 2)
    engine.kron(a, a) shouldBe vec[Rational](1, 2, 2, 4)
  }

}
