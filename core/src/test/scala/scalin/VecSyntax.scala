package scalin

import spire.math.Rational

class VecSyntax extends ScalinSuite {

  import immutable.dense._

  test("build.tabulate") {
    Vec.tabulate(2)(i => Rational(i)) shouldBe Vec[Rational](0, 1)
  }

  test("build.fill") {
    Vec.fill(2)(Rational(1)) shouldBe Vec[Rational](1, 1)
  }

  test("Vec.cat") {
    val a = Vec[Rational](0, 1)
    val b = Vec[Rational](2, 3)
    val res = Vec[Rational](0, 1, 2, 3)
    (a cat b) shouldBe res
  }

  test("Vec.count") {
    val a = Vec[Rational](0, 1, -1, 2, -2)
    a.count(_ > 0) shouldBe 2
  }

  test("Vec.flatMap") {
    val a = Vec[Rational](1, 2)
    a.flatMap(x => Vec[Rational](x, 0)) shouldBe Vec[Rational](1, 0, 2, 0)
  }

  test("Vec.flatten") {
    val a: Vec[Vec[Rational]] = Vec.tabulate(2)(i => Vec[Rational](0, 1))
    a.flatten shouldBe Vec[Rational](0, 1, 0, 1)
  }

  test("Vec.fold") {
    val a = Vec[Rational](1, 2, 3, 4)
    a.fold(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("Vec.map") {
    val a = Vec[Rational](2, 3, 4)
    a.map(_ * 2) shouldBe Vec[Rational](4, 6, 8)
  }

  test("Mat.colSeq") {
    val a = Mat.rowMajor(2, 2)(1, 2,
      3, 4)
    a.colSeq shouldBe IndexedSeq(Vec[Rational](1, 3), Vec[Rational](2, 4))
  }

  test("Mat.rowSeq") {
    val a = Mat.rowMajor(2, 2)(1, 2,
      3, 4)
    a.rowSeq shouldBe IndexedSeq(Vec[Rational](1, 2), Vec[Rational](3, 4))
  }

  test("Vec(range)") {
    val a = Vec[Rational](0, 1, 2, 3)
    a(1 to 2) shouldBe Vec[Rational](1, 2)
  }

  test("Mat(range)") {
    val a = Mat.rowMajor(2, 2)(1, 2,
      3, 4)
    a(1 to 2) shouldBe Vec[Rational](3, 2)
  }

  test("Mat.diag") {
    Mat.rowMajor(2, 2)(1, 0, 0, 2).diag shouldBe Vec[Rational](1, 2)
  }

  test("Mat(i, range)") {
    val a = Mat.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
      7, 8, 9)
    a(1, 1 to 2) shouldBe Vec[Rational](5, 6)
  }

  test("Mat(range, i") {
    val a = Mat.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
      7, 8, 9)
    a(1 to 2, 1) shouldBe Vec[Rational](5, 8)
  }

  test("Vec pw_== (scalar)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val res = Vec[Boolean](false, false, true, true)
    (a pw_== Rational(2)) shouldBe res
  }

  test("Vec pw_== (vec)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val b = Vec[Rational](0, 2, 1, 2)
    val res = Vec[Boolean](true, false, false, true)
    (a pw_== b) shouldBe res
  }

  test("Vec pw_!= (scalar)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val res = Vec[Boolean](true, true, false, false)
    (a pw_!= Rational(2)) shouldBe res
  }

  test("Vec pw_!= (vec)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val b = Vec[Rational](0, 2, 1, 2)
    val res = Vec[Boolean](false, true, true, false)
    (a pw_!= b) shouldBe res
  }

  test("VecEngine.pointwiseEqv (scalar)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val res = Vec[Boolean](false, false, true, true)
    (a pw_=== Rational(2)) shouldBe res
  }

  test("Vec pw_=== (vec)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val b = Vec[Rational](0, 2, 1, 2)
    val res = Vec[Boolean](true, false, false, true)
    (a pw_=== b) shouldBe res
  }

  test("Vec pw_=!= (scalar)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val res = Vec[Boolean](true, true, false, false)
    (a pw_=!= Rational(2)) shouldBe res
  }

  test("Vec pw_=!= (vec)") {
    val a = Vec[Rational](0, 1, 2, 2)
    val b = Vec[Rational](0, 2, 1, 2)
    val res = Vec[Boolean](false, true, true, false)
    (a pw_=!= b) shouldBe res
  }

  /* TODO
  test("VecEngine.eqv") {
    val a = vec[Rational](0, 1, 2)
    val b = vec[Rational](0, 1, 2)
    val c = vec[Rational](0, 1, 1)
    (a === b) shouldBe true
    (a === c) shouldBe false
  }
   */

}
