package scalin.algebra

import spire.math.Rational

import scalin.{Mat, ScalinSuite, immutable}
import scalin.immutable.Vec


class VecRingTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]
  implicit val matEngine: MatEngine[Rational, Mat[Rational]] = immutable.dense.matEngine[Rational]

  test("zeros") {
    engine.zeros(3) shouldBe vec[Rational](0, 0, 0)
  }

  test("plus") {
    val a = vec[Rational](0, 1, 2)
    val b = vec[Rational](2, 2, 2)
    val res = vec[Rational](2, 3, 4)
    engine.plus(a, b) shouldBe res
  }

  test("minus") {
    val a = vec[Rational](2, 2, 2)
    val b = vec[Rational](0, 1, 2)
    val res = vec[Rational](2, 1, 0)
    engine.minus(a, b) shouldBe res
  }

  test("negate") {
    val a = vec[Rational](2, -1, 0)
    val res = vec[Rational](-2, 1, 0)
    engine.negate(a) shouldBe res
  }

  test("pointwisePlus") {
    val a = vec[Rational](1, 2, 3)
    val res = vec[Rational](2, 3, 4)
    engine.pointwisePlus(a, Rational(1)) shouldBe res
  }

  test("pointwiseMinus") {
    val a = vec[Rational](2, 3, 4)
    val res = vec[Rational](1, 2, 3)
    engine.pointwiseMinus(a, Rational(1)) shouldBe res
  }

  test("nnz") {
    val a = vec[Rational](2, 0, 1, 0, 0)
    engine.nnz(a) shouldBe 3
  }

  test("sum") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.sum(a) shouldBe Rational(10)
  }

  test("times mat-vec") {
    val a = matEngine.rowMajor(2, 2)(1, 2, 2, 1)
    val b = vec[Rational](1, 1)
    engine.times(a, b) shouldBe vec[Rational](3, 3)
  }

  test("times vec-mat") {
    val a = vec[Rational](1, 1)
    val b = matEngine.rowMajor(2, 2)(1, 2, 2, 1)
    engine.times(a, b) shouldBe vec[Rational](3, 3)
  }

  test("dot") {
    val a = vec[Rational](1, 2)
    val b = vec[Rational](2, 1)
  }

}
