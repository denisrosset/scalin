package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Mat

class MatEngineTests extends ScalinSuite {

  implicit val engine: MatField[Rational, Mat[Rational]] = immutable.dense.matField[Rational]
  implicit val engineB: MatEngine[Boolean, Mat[Boolean]] = immutable.dense.matEngine[Boolean]
  implicit val vecEngine: VecEngine[Rational, Vec[Rational]] = immutable.dense.vecEngine[Rational]

  test("tabulate") {
    engine.tabulate(2, 2)( (i, j) => Rational(i+j)) shouldBe rowMajor[Rational](2, 2)(0, 1, 1, 2)
  }

  test("fill") {
    engine.fill(2, 2)(Rational(1)) shouldBe rowMajor[Rational](2, 2)(1, 1, 1, 1)
  }

  test("fillConstant") {
    engine.fillConstant(2, 2)(Rational(1)) shouldBe rowMajor[Rational](2, 2)(1, 1, 1, 1)
  }

  test("colMajor") {
    engine.colMajor(2, 2)(1, 2, 3, 4) shouldBe rowMajor[Rational](2, 2)(1, 3, 2, 4)
  }

  test("rowMajor") {
    engine.rowMajor(2, 2)(1, 2, 3, 4) shouldBe colMajor[Rational](2, 2)(1, 3, 2, 4)
  }

  test("rowMat") {
    engine.rowMat(1, 2, 3) shouldBe rowMajor[Rational](1, 3)(1, 2, 3)
  }

  test("colMat") {
    engine.colMat(1, 2, 3) shouldBe rowMajor[Rational](3, 1)(1, 2, 3)
  }

  test("toRowMat") {
    engine.toRowMat(vec[Rational](1, 2, 3)) shouldBe rowMajor[Rational](1, 3)(1, 2, 3)
  }

  test("toColMat") {
    engine.toColMat(vec[Rational](1, 2, 3)) shouldBe colMajor[Rational](3, 1)(1, 2, 3)
  }

  test("fromMat") {
    engine.fromMat(rowMajor[Rational](2,2)(1,2,3,4)) shouldBe rowMajor[Rational](2,2)(1,2,3,4)
  }

  test("count") {
    engine.count(rowMajor[Rational](2,2)(1,2,3,4))(_ > 2) shouldBe 2
  }

  test("flatten") {
    val a: Mat[Mat[Rational]] = immutable.DenseMat.tabulate(2, 2)((i, j) => rowMajor[Rational](1, 1)(i+j))
    engine.flatten(a) shouldBe rowMajor[Rational](2, 2)(0, 1, 1, 2)
  }

  test("flatMap") {
    val a = rowMajor[Rational](2,2)(0,1,1,2)
    engine.flatMap(a)(x => rowMajor[Rational](1, 1)(x + 1)) shouldBe rowMajor[Rational](2, 2)(1, 2, 2, 3)
  }

  test("fold") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    engine.fold(a)(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("map") {
    val a = rowMajor[Rational](2, 2)(0, 1, 2, 3)
    engine.map(a)(_ * 2) shouldBe rowMajor[Rational](2, 2)(0, 2, 4, 6)
  }

  test("horzcat") {
    val a = rowMajor[Rational](2, 2)(0, 1, 2, 3)
    val b = rowMajor[Rational](2, 2)(4, 5, 6, 7)
    val res = rowMajor[Rational](2, 4)(0, 1, 4, 5, 2, 3, 6, 7)
    engine.horzcat(a, b) shouldBe res
  }

  test("vertcat") {
    val a = colMajor[Rational](2, 2)(0, 1, 2, 3)
    val b = colMajor[Rational](2, 2)(4, 5, 6, 7)
    val res = colMajor[Rational](4, 2)(0, 1, 4, 5, 2, 3, 6, 7)
    engine.vertcat(a, b) shouldBe res
  }

  test("slice") {
    val a = rowMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    engine.slice(a, 1 to 2, 1 to 2) shouldBe rowMajor[Rational](2, 2)(4, 5, 7, 8)
  }

  test("t") {
    val a = rowMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    val res = colMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    engine.t(a) shouldBe res
  }

  test("reshape") {
    val a = vec[Rational](1, 2, 3, 4)
    val res = colMajor[Rational](2, 2)(1, 2, 3, 4)
    engine.reshape(a, 2, 2) shouldBe res
  }

  test("pointwiseEqual (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](false, false, true, true)
    engineB.pointwiseEqual(a, Rational(2)) shouldBe res
  }

  test("pointwiseEqual (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](true, false, false, true)
    engineB.pointwiseEqual(a, b) shouldBe res
  }

  test("pointwiseNotEqual (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](true, true, false, false)
    engineB.pointwiseNotEqual(a, Rational(2)) shouldBe res
  }

  test("pointwiseNotEqual (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](false, true, true, false)
    engineB.pointwiseNotEqual(a, b) shouldBe res
  }

  test("pointwiseEqv (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](false, false, true, true)
    engineB.pointwiseEqv(a, Rational(2)) shouldBe res
  }

  test("pointwiseEqv (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](true, false, false, true)
    engineB.pointwiseEqv(a, b) shouldBe res
  }

  test("pointwiseNotEqv (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](true, true, false, false)
    engineB.pointwiseNeqv(a, Rational(2)) shouldBe res
  }

  test("pointwiseNotEqv (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](false, true, true, false)
    engineB.pointwiseNeqv(a, b) shouldBe res
  }

  test("eqv") {
    val a = rowMat[Rational](0, 1, 2)
    val b = rowMat[Rational](0, 1, 2)
    val c = rowMat[Rational](0, 1, 1)
    engine.eqv(a, b) shouldBe true
    engine.eqv(a, c) shouldBe false
  }

  test("ones") {
    engine.ones(2, 2) shouldBe rowMajor[Rational](2, 2)(1, 1, 1, 1)
  }

  test("product") {
    engine.product(rowMajor[Rational](2, 2)(1,2,3,4)) shouldBe Rational(24)
  }

  test("times") {
    val a = rowMat[Rational](1, 2, 3, 4)
    engine.times(a, Rational(2)) shouldBe rowMat[Rational](2, 4, 6, 8)
    engine.times(Rational(2), a) shouldBe rowMat[Rational](2, 4, 6, 8)
    // TODO: test with a noncommutative ring
  }

  test("pointwiseTimes") {
    val a = rowMat[Rational](1, 2, 3, 4)
    engine.pointwiseTimes(a, a) shouldBe rowMat[Rational](1, 4, 9, 16)
  }

  test("dyad") {
    val a = vec[Rational](1, 2)
    val res = rowMajor[Rational](2, 2)(1, 2, 2, 4)
    engine.dyad(a, a) shouldBe res
  }

  test("kron") {
    val a = colMat[Rational](1, 2)
    engine.kron(a, a) shouldBe colMat[Rational](1, 2, 2, 4)
  }

  test("zeros") {
    engine.zeros(2, 2) shouldBe rowMajor[Rational](2, 2)(0, 0, 0, 0)
  }

  test("eye") {
    engine.eye(2) shouldBe rowMajor[Rational](2, 2)(1, 0, 0, 1)
  }

  test("toDiagMat") {
    engine.toDiagMat(vec[Rational](1, 2)) shouldBe rowMajor[Rational](2, 2)(1, 0, 0, 2)
  }

  test("plus") {
    val a = colMat[Rational](0, 1, 2)
    val b = colMat[Rational](2, 2, 2)
    val res = colMat[Rational](2, 3, 4)
    engine.plus(a, b) shouldBe res
  }

  test("minus") {
    val a = colMat[Rational](2, 2, 2)
    val b = colMat[Rational](0, 1, 2)
    val res = colMat[Rational](2, 1, 0)
    engine.minus(a, b) shouldBe res
  }

    test("negate") {
    val a = colMat[Rational](2, -1, 0)
    val res = colMat[Rational](-2, 1, 0)
    engine.negate(a) shouldBe res
  }

  test("pointwisePlus") {
    val a = colMat[Rational](1, 2, 3)
    val res = colMat[Rational](2, 3, 4)
    engine.pointwisePlus(a, Rational(1)) shouldBe res
  }

  test("pointwiseMinus") {
    val a = colMat[Rational](2, 3, 4)
    val res = colMat[Rational](1, 2, 3)
    engine.pointwiseMinus(a, Rational(1)) shouldBe res
  }

  test("nnz") {
    val a = rowMajor[Rational](2, 2)(0, 1, 0, 1)
    engine.nnz(a) shouldBe 2
  }

  test("sum") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    engine.sum(a) shouldBe Rational(10)
  }

  test("trace") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    engine.trace(a) shouldBe Rational(5)
  }

  test("times (mat, mat)") {
    val a = rowMajor[Rational](2, 4)(1, 3, 5, 7,
      2, 4, 6, 8)
    val b = rowMajor[Rational](4, 3)(1, 8, 9,
      2, 7, 10,
      3, 6, 11,
      4, 5, 12)
    val res = rowMajor[Rational](2, 3)(50, 94, 178,
      60, 120, 220)
    engine.times(a, b) shouldBe res
  }

  test("frobenius product") {
    val a = rowMajor[Rational](2, 3)(2, 0, 6,
      1, -1, 2)
    val b = rowMajor[Rational](2, 3)(8, -3, 2,
      4, 1, -5)
    engine.frobenius(a, b) shouldBe Rational(21)
  }

  test("determinant") {
    // Wikipedia example: determinant of 3x3 matrix
    rowMajor[Rational](3,3)(
      -2, 2,-3,
      -1, 1, 3,
      2, 0,-1).determinant shouldBe Rational(18)
  }

}

