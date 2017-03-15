package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Vec

class VecEuclideanRingTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]

  test("gcd") {
    val a = vec[Rational](2, 4, -2)
    engine.gcd(a) shouldBe Rational(2)
  }

  test("lcm") {
    val a = vec[Rational](2, 3, 6)
    engine.lcm(a) shouldBe Rational(6)
  }

}
