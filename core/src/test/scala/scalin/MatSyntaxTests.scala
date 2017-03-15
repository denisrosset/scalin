package scalin

import spire.math.Rational

class MatSyntaxTests extends ScalinSuite {

  import immutable.dense._

  test("tabulate") {
    tabulate(2, 2)( (i, j) => Rational(i+j)) shouldBe rowMajor[Rational](2, 2)(0, 1, 1, 2)
  }

  test("colMajor") {
    colMajor(2, 2)(1, 2, 3, 4) shouldBe rowMajor[Rational](2, 2)(1, 3, 2, 4)
  }

  test("rowMajor") {
    rowMajor(2, 2)(1, 2, 3, 4) shouldBe colMajor[Rational](2, 2)(1, 3, 2, 4)
  }

  test("rowMat") {
    rowMat(1, 2, 3) shouldBe rowMajor[Rational](1, 3)(1, 2, 3)
  }

  test("colMat") {
    colMat(1, 2, 3) shouldBe rowMajor[Rational](3, 1)(1, 2, 3)
  }

  test("toRowMat") {
    vec[Rational](1, 2, 3).toRowMat shouldBe rowMajor[Rational](1, 3)(1, 2, 3)
  }

  test("toColMat") {
    vec[Rational](1, 2, 3).toColMat shouldBe colMajor[Rational](3, 1)(1, 2, 3)
  }

  test("count") {
    rowMajor[Rational](2,2)(1,2,3,4).count(_ > 2) shouldBe 2
  }

  test("flatten") {
    val a: Mat[Mat[Rational]] = immutable.DenseMat.tabulate(2, 2)((i, j) => rowMajor[Rational](1, 1)(i+j))
    a.flatten shouldBe rowMajor[Rational](2, 2)(0, 1, 1, 2)
  }

  test("flatMap") {
    val a = rowMajor[Rational](2,2)(0,1,1,2)
    a.flatMap(x => rowMajor[Rational](1, 1)(x + 1)) shouldBe rowMajor[Rational](2, 2)(1, 2, 2, 3)
  }

  test("fold") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    a.fold(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("map") {
    val a = rowMajor[Rational](2, 2)(0, 1, 2, 3)
    a.map(_ * 2) shouldBe rowMajor[Rational](2, 2)(0, 2, 4, 6)
  }

  test("horzcat") {
    val a = rowMajor[Rational](2, 2)(0, 1, 2, 3)
    val b = rowMajor[Rational](2, 2)(4, 5, 6, 7)
    val res = rowMajor[Rational](2, 4)(0, 1, 4, 5, 2, 3, 6, 7)
    (a horzcat b) shouldBe res
  }

  test("vertcat") {
    val a = colMajor[Rational](2, 2)(0, 1, 2, 3)
    val b = colMajor[Rational](2, 2)(4, 5, 6, 7)
    val res = colMajor[Rational](4, 2)(0, 1, 4, 5, 2, 3, 6, 7)
    (a vertcat b) shouldBe res
  }

  test("slice") {
    val a = rowMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    a(1 to 2, 1 to 2) shouldBe rowMajor[Rational](2, 2)(4, 5, 7, 8)
  }

  test("t") {
    val a = rowMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    val res = colMajor[Rational](3, 3)(0, 1, 2,
      3, 4, 5,
      6, 7, 8)
    a.t shouldBe res
  }

  test("reshape") {
    val a = vec[Rational](1, 2, 3, 4)
    val res = colMajor[Rational](2, 2)(1, 2, 3, 4)
    a.reshape(2, 2) shouldBe res
  }

  test("pointwiseEqual (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](false, false, true, true)
    (a.pointwise == Rational(2)) shouldBe res
  }

  test("pointwiseEqual (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](true, false, false, true)
    (a.pointwise == b) shouldBe res
  }

  test("pointwiseNotEqual (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](true, true, false, false)
    (a.pointwise != Rational(2)) shouldBe res
  }

  test("pointwiseNotEqual (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](false, true, true, false)
    (a.pointwise != b) shouldBe res
  }

  test("pointwiseEqv (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](false, false, true, true)
    (a.pointwise === Rational(2)) shouldBe res
  }

  test("pointwiseEqv (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](true, false, false, true)
    (a.pointwise === b) shouldBe res
  }

  test("pointwiseNotEqv (scalar)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val res = rowMat[Boolean](true, true, false, false)
    (a.pointwise =!= Rational(2)) shouldBe res
  }

  test("pointwiseNotEqv (vec)") {
    val a = rowMat[Rational](0, 1, 2, 2)
    val b = rowMat[Rational](0, 2, 1, 2)
    val res = rowMat[Boolean](false, true, true, false)
    (a.pointwise =!= b) shouldBe res
  }

  /* TODO
  test("eqv") {
    val a = rowMat[Rational](0, 1, 2)
    val b = rowMat[Rational](0, 1, 2)
    val c = rowMat[Rational](0, 1, 1)
    engine.eqv(a, b) shouldBe true
    engine.eqv(a, c) shouldBe false
  }
   */

  test("ones") {
    ones[Rational](2, 2) shouldBe rowMajor[Rational](2, 2)(1, 1, 1, 1)
  }

  test("product") {
    rowMajor[Rational](2, 2)(1,2,3,4).product shouldBe Rational(24)
  }

  test("times") {
    val a = rowMat[Rational](1, 2, 3, 4)
    (a * Rational(2)) shouldBe rowMat[Rational](2, 4, 6, 8)
    (Rational(2) *: a) shouldBe rowMat[Rational](2, 4, 6, 8)
    // TODO: test with a noncommutative ring
  }

  test("pointwiseTimes") {
    val a = rowMat[Rational](1, 2, 3, 4)
    (a.pointwise * a) shouldBe rowMat[Rational](1, 4, 9, 16)
  }
  test("dyad") {
    val a = vec[Rational](1, 2)
    val res = rowMajor[Rational](2, 2)(1, 2, 2, 4)
    (a dyad a) shouldBe res
  }

  test("kron") {
    val a = colMat[Rational](1, 2)
    (a kron a) shouldBe colMat[Rational](1, 2, 2, 4)
  }

  test("zeros") {
    zeros[Rational](2, 2) shouldBe rowMajor[Rational](2, 2)(0, 0, 0, 0)
  }

  test("eye") {
    eye[Rational](2) shouldBe rowMajor[Rational](2, 2)(1, 0, 0, 1)
  }

  test("toDiagMat") {
    vec[Rational](1, 2).toDiagMat shouldBe rowMajor[Rational](2, 2)(1, 0, 0, 2)
  }

  test("plus") {
    val a = colMat[Rational](0, 1, 2)
    val b = colMat[Rational](2, 2, 2)
    val res = colMat[Rational](2, 3, 4)
    (a + b) shouldBe res
  }

  test("minus") {
    val a = colMat[Rational](2, 2, 2)
    val b = colMat[Rational](0, 1, 2)
    val res = colMat[Rational](2, 1, 0)
    (a - b) shouldBe res
  }

  test("negate") {
    val a = colMat[Rational](2, -1, 0)
    val res = colMat[Rational](-2, 1, 0)
    (-a) shouldBe res
  }

  test("pointwisePlus") {
    val a = colMat[Rational](1, 2, 3)
    val res = colMat[Rational](2, 3, 4)
    (a.pointwise + Rational(1)) shouldBe res
  }

  test("pointwiseMinus") {
    val a = colMat[Rational](2, 3, 4)
    val res = colMat[Rational](1, 2, 3)
    (a.pointwise - Rational(1)) shouldBe res
  }

  test("nnz") {
    val a = rowMajor[Rational](2, 2)(0, 1, 0, 1)
    a.nnz shouldBe 2
  }

  test("sum") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    a.sum shouldBe Rational(10)
  }

  test("trace") {
    val a = rowMajor[Rational](2, 2)(1, 2, 3, 4)
    a.trace shouldBe Rational(5)
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
    (a * b) shouldBe res
  }

  test("frobenius product") {
    val a = rowMajor[Rational](2, 3)(2, 0, 6,
      1, -1, 2)
    val b = rowMajor[Rational](2, 3)(8, -3, 2,
      4, 1, -5)
    (a frobenius b) shouldBe Rational(21)
  }

  /* TODO
  test("determinant") {
    // Wikipedia example: determinant of 3x3 matrix
    rowMajor[Rational](3,3)(
      -2, 2,-3,
      -1, 1, 3,
      2, 0,-1).determinant shouldBe Rational(18)
  }

  test("rank") {
    val a = rowMajor[Rational](4,4)(
      1,3,1,4,
      2,7,3,9,
      1,5,3,1,
      1,2,0,8)
    a.rank shouldBe 3
  }
   */

  test("gcd") {
    val a = rowMat[Rational](2, 4, -2)
    a.gcd shouldBe Rational(2)
  }

  test("lcm") {
    val a = rowMat[Rational](2, 3, 6)
    a.lcm shouldBe Rational(6)
  }

  /* TODO
  test("orthogonalized") {
    val a = eye[Rational](2)
    a.orthogonalized shouldBe a
  }*/

  /* TODO

  test("inverse") {
    val a = rowMajor[Rational](2, 2)(1, 0, 0, 2)
    val res = rowMajor[Rational](2, 2)(Rational.one, Rational.zero, Rational.zero, Rational(1, 2))
    a.inverse shouldBe res
  }
   */

  test("pointwiseDiv") {
    val a = rowMat[Rational](2, 4, 4, 2)
    val b = rowMat[Rational](2, 1, 4, 1)
    val res = rowMat[Rational](1, 4, 1, 2)
    (a.pointwise / b) shouldBe res
  }

  test("div") {
    val a = rowMat[Rational](2, 4, 4, 6)
    val res = rowMat[Rational](1, 2, 2, 3)
    (a / Rational(2)) shouldBe res
  }

}
