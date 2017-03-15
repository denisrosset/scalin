package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Vec

class VecEngineTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]
  implicit val engineB: VecEngine[Boolean, Vec[Boolean]] = immutable.dense.vecEngine[Boolean]
  implicit val matEngine: MatEngine[Rational, Mat[Rational]] = immutable.dense.matEngine[Rational]

  def r[A](a: A)(implicit f: A => Rational): Rational = f(a)

  test("VecEngine.empty") {
    engine.empty.length == 0
  }

  test("VecEngine.tabulate") {
    engine.tabulate(2)(i => r(i)) shouldBe vec[Rational](0, 1)
  }

  test("VecEngine.fill") {
    engine.fill(2)(r(1)) shouldBe vec(r(1), r(1))
  }

  test("VecEngine.fromSeq") {
    engine.fromSeq(Seq(r(0), r(1))) shouldBe vec[Rational](0, 1)
  }

  test("VecEngine.fromVec") {
    engine.fromVec(vec[Rational](0, 1)) shouldBe vec[Rational](0, 1)
  }

  test("VecEngine.cat") {
    val a = vec[Rational](0, 1)
    val b = vec[Rational](2, 3)
    val res = vec[Rational](0, 1, 2, 3)
    engine.cat(a, b) shouldBe res
  }

  test("VecEngine.count") {
    val a = vec[Rational](0, 1, -1, 2, -2)
    engine.count(a)(_ > 0) shouldBe 2
  }

  test("VecEngine.flatMap") {
    val a = vec[Rational](1, 2)
    engine.flatMap(a)(x => vec[Rational](x, 0)) shouldBe vec[Rational](1, 0, 2, 0)
  }

  test("VecEngine.flatten") {
    val a: Vec[Vec[Rational]] = immutable.DenseVec.tabulate(2)(i => vec[Rational](0, 1))
    engine.flatten(a) shouldBe vec[Rational](0, 1, 0, 1)
  }

  test("VecEngine.fold") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.fold(a)(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("VecEngine.map") {
    val a = vec[Rational](2, 3, 4)
    engine.map(a)(_ * 2) shouldBe vec[Rational](4, 6, 8)
  }

  test("VecEngine.colSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
                                     3, 4)
    engine.colSeq(a) shouldBe IndexedSeq(vec[Rational](1, 3), vec[Rational](2, 4))
  }

  test("VecEngine.rowSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
                                     3, 4)
    engine.rowSeq(a) shouldBe IndexedSeq(vec[Rational](1, 2), vec[Rational](3, 4))
  }

  test("VecEngine.slice") {
    val a = vec[Rational](0, 1, 2, 3)
    engine.slice(a, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](1, 2)
  }

  test("VecEngine.slice (matrix") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
      3, 4)
    engine.slice(a, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](3, 2)
  }

  test("VecEngine.diag") {
    engine.diag(matEngine.rowMajor(2, 2)(1, 0, 0, 2)) shouldBe vec[Rational](1, 2)
  }

  test("VecEngine.rowSlice") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
    7, 8, 9)
    engine.rowSlice(a, 1, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](5, 6)
  }

  test("VecEngine.colSlice") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
    4, 5, 6,
    7, 8, 9)
    engine.colSlice(a, Subscript.arrayWrap(Array(1, 2)), 1) shouldBe vec[Rational](5, 8)
  }

  test("VecEngine.pointwiseEqual (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    engineB.pointwiseEqual(a, Rational(2)) shouldBe res
  }

  test("VecEngine.pointwiseEqual (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    engineB.pointwiseEqual(a, b) shouldBe res
  }

  test("VecEngine.pointwiseNotEqual (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    engineB.pointwiseNotEqual(a, Rational(2)) shouldBe res
  }

  test("VecEngine.pointwiseNotEqual (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
    engineB.pointwiseNotEqual(a, b) shouldBe res
  }

  test("VecEngine.pointwiseEqv (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    engineB.pointwiseEqv(a, Rational(2)) shouldBe res
  }

  test("VecEngine.pointwiseEqv (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    engineB.pointwiseEqv(a, b) shouldBe res
  }

  test("VecEngine.pointwiseNotEqv (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    engineB.pointwiseNeqv(a, Rational(2)) shouldBe res
  }

  test("VecEngine.pointwiseNotEqv (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
    engineB.pointwiseNeqv(a, b) shouldBe res
  }

  test("VecEngine.eqv") {
    val a = vec[Rational](0, 1, 2)
    val b = vec[Rational](0, 1, 2)
    val c = vec[Rational](0, 1, 1)
    engine.eqv(a, b) shouldBe true
    engine.eqv(a, c) shouldBe false
  }



}
