package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Vec

class VecEngineTests extends ScalinSuite {

  implicit val engine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]
  implicit val engineB: VecEngine[Boolean, Vec[Boolean]] = immutable.dense.vecEngine[Boolean]
  implicit val matEngine: MatEngine[Rational, Mat[Rational]] = immutable.dense.matEngine[Rational]

  test("empty") {
    engine.empty.length == 0
  }

  test("tabulate") {
    engine.tabulate(2)(i => Rational(i)) shouldBe vec[Rational](0, 1)
  }

  test("fill") {
    engine.fill(2)(Rational(1)) shouldBe vec[Rational](1, 1)
  }

  test("fillConstant") {
    engine.fillConstant(2)(Rational(1)) shouldBe vec[Rational](1, 1)
  }

  test("fromSeq") {
    engine.fromSeq(Seq(Rational(0), Rational(1))) shouldBe vec[Rational](0, 1)
  }

  test("fromVec") {
    engine.fromVec(vec[Rational](0, 1)) shouldBe vec[Rational](0, 1)
  }

  test("cat") {
    val a = vec[Rational](0, 1)
    val b = vec[Rational](2, 3)
    val res = vec[Rational](0, 1, 2, 3)
    engine.cat(a, b) shouldBe res
  }

  test("count") {
    val a = vec[Rational](0, 1, -1, 2, -2)
    engine.count(a)(_ > 0) shouldBe 2
  }

  test("flatMap") {
    val a = vec[Rational](1, 2)
    engine.flatMap(a)(x => vec[Rational](x, 0)) shouldBe vec[Rational](1, 0, 2, 0)
  }

  test("flatten") {
    val a: Vec[Vec[Rational]] = immutable.DenseVec.tabulate(2)(i => vec[Rational](0, 1))
    engine.flatten(a) shouldBe vec[Rational](0, 1, 0, 1)
  }

  test("fold") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.fold(a)(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("map") {
    val a = vec[Rational](2, 3, 4)
    engine.map(a)(_ * 2) shouldBe vec[Rational](4, 6, 8)
  }

  test("colSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
                                     3, 4)
    engine.colSeq(a) shouldBe IndexedSeq(vec[Rational](1, 3), vec[Rational](2, 4))
  }

  test("rowSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
                                     3, 4)
    engine.rowSeq(a) shouldBe IndexedSeq(vec[Rational](1, 2), vec[Rational](3, 4))
  }

  test("slice") {
    val a = vec[Rational](0, 1, 2, 3)
    engine.slice(a, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](1, 2)
  }

  test("slice (matrix") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
      3, 4)
    engine.slice(a, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](3, 2)
  }

  test("diag") {
    engine.diag(matEngine.rowMajor(2, 2)(1, 0, 0, 2)) shouldBe vec[Rational](1, 2)
  }

  test("rowSlice") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
    7, 8, 9)
    engine.rowSlice(a, 1, Subscript.arrayWrap(Array(1, 2))) shouldBe vec[Rational](5, 6)
  }

  test("colSlice") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
    4, 5, 6,
    7, 8, 9)
    engine.colSlice(a, Subscript.arrayWrap(Array(1, 2)), 1) shouldBe vec[Rational](5, 8)
  }

  test("pointwiseEqual (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    engineB.pointwiseEqual(a, Rational(2)) shouldBe res
  }

  test("pointwiseEqual (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    engineB.pointwiseEqual(a, b) shouldBe res
  }

  test("pointwiseNotEqual (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    engineB.pointwiseNotEqual(a, Rational(2)) shouldBe res
  }

  test("pointwiseNotEqual (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
    engineB.pointwiseNotEqual(a, b) shouldBe res
  }

  test("pointwiseEqv (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    engineB.pointwiseEqv(a, Rational(2)) shouldBe res
  }

  test("pointwiseEqv (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    engineB.pointwiseEqv(a, b) shouldBe res
  }

  test("pointwiseNotEqv (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    engineB.pointwiseNeqv(a, Rational(2)) shouldBe res
  }

  test("pointwiseNotEqv (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
    engineB.pointwiseNeqv(a, b) shouldBe res
  }

  test("eqv") {
    val a = vec[Rational](0, 1, 2)
    val b = vec[Rational](0, 1, 2)
    val c = vec[Rational](0, 1, 1)
    engine.eqv(a, b) shouldBe true
    engine.eqv(a, c) shouldBe false
  }

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

  test("ones") {
    engine.ones(3) shouldBe vec[Rational](1, 1, 1)
  }

  test("product") {
    engine.product(vec[Rational](1, 2, 3, 4)) shouldBe Rational(24)
  }

  test("times") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.times(a, Rational(2)) shouldBe vec[Rational](2, 4, 6, 8)
    engine.times(Rational(2), a) shouldBe vec[Rational](2, 4, 6, 8)
    // TODO: test with a noncommutative ring
  }

  test("pointwiseTimes") {
    val a = vec[Rational](1, 2, 3, 4)
    engine.pointwiseTimes(a, a) shouldBe vec[Rational](1, 4, 9, 16)
  }

  test("kron") {
    val a = vec[Rational](1, 2)
    engine.kron(a, a) shouldBe vec[Rational](1, 2, 2, 4)
  }

  test("gcd") {
    val a = vec[Rational](2, 4, -2)
    engine.gcd(a) shouldBe Rational(2)
  }

  test("lcm") {
    val a = vec[Rational](2, 3, 6)
    engine.lcm(a) shouldBe Rational(6)
  }

  test("pointwiseDiv") {
    val a = vec[Rational](2, 4, 4, 2)
    val b = vec[Rational](2, 1, 4, 1)
    val res = vec[Rational](1, 4, 1, 2)
    engine.pointwiseDiv(a, b) shouldBe res
  }

  test("div") {
    val a = vec[Rational](2, 4, 4, 6)
    val res = vec[Rational](1, 2, 2, 3)
    engine.div(a, Rational(2)) shouldBe res
  }


}
