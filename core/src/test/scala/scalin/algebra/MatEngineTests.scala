package scalin
package algebra

import spire.math.Rational

import scalin.immutable.Mat

class MatEngineTests extends ScalinSuite {

  implicit val engine: MatEngine[Rational, Mat[Rational]] = immutable.dense.matEngine[Rational]
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
  
}

