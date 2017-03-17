package scalin

import spire.math.Rational

class VecSyntax extends ScalinSuite {

  import immutable.dense._

  test("build.tabulate") {
    tabulate(2)(i => Rational(i)) shouldBe vec[Rational](0, 1)
  }

  test("build.fill") {
    fill(2)(Rational(1)) shouldBe vec[Rational](1, 1)
  }

  test("Vec.cat") {
    val a = vec[Rational](0, 1)
    val b = vec[Rational](2, 3)
    val res = vec[Rational](0, 1, 2, 3)
    (a cat b) shouldBe res
  }

  test("Vec.count") {
    val a = vec[Rational](0, 1, -1, 2, -2)
    a.count(_ > 0) shouldBe 2
  }

  test("Vec.flatMap") {
    val a = vec[Rational](1, 2)
    a.flatMap(x => vec[Rational](x, 0)) shouldBe vec[Rational](1, 0, 2, 0)
  }

  test("Vec.flatten") {
    val a: immutable.Vec[immutable.Vec[Rational]] = immutable.DenseVec.tabulate(2)(i => vec[Rational](0, 1))
    a.flatten shouldBe vec[Rational](0, 1, 0, 1)
  }

  test("Vec.fold") {
    val a = vec[Rational](1, 2, 3, 4)
    a.fold(Rational.one)(_ * _) shouldBe Rational(24)
  }

  test("Vec.map") {
    val a = vec[Rational](2, 3, 4)
    a.map(_ * 2) shouldBe vec[Rational](4, 6, 8)
  }

  test("Mat.colSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
      3, 4)
    a.colSeq shouldBe IndexedSeq(vec[Rational](1, 3), vec[Rational](2, 4))
  }

  test("Mat.rowSeq") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
      3, 4)
    a.rowSeq shouldBe IndexedSeq(vec[Rational](1, 2), vec[Rational](3, 4))
  }

  test("Vec(range)") {
    val a = vec[Rational](0, 1, 2, 3)
    a(1 to 2) shouldBe vec[Rational](1, 2)
  }

  test("Mat(range)") {
    val a = matEngine.rowMajor(2, 2)(1, 2,
      3, 4)
    a(1 to 2) shouldBe vec[Rational](3, 2)
  }

  test("Mat.diag") {
    matEngine.rowMajor(2, 2)(1, 0, 0, 2).diag shouldBe vec[Rational](1, 2)
  }

  test("Mat(i, range)") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
      7, 8, 9)
    a(1, 1 to 2) shouldBe vec[Rational](5, 6)
  }

  test("Mat(range, i") {
    val a = matEngine.rowMajor(3, 3)(1, 2, 3,
      4, 5, 6,
      7, 8, 9)
    a(1 to 2, 1) shouldBe vec[Rational](5, 8)
  }

  test("Vec pw_== (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    (a pw_== Rational(2)) shouldBe res
  }

  test("Vec pw_== (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    (a pw_== b) shouldBe res
  }

  test("Vec pw_!= (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    (a pw_!= Rational(2)) shouldBe res
  }

  test("Vec pw_!= (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
    (a pw_!= b) shouldBe res
  }

  test("VecEngine.pointwiseEqv (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](false, false, true, true)
    (a pw_=== Rational(2)) shouldBe res
  }

  test("Vec pw_=== (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](true, false, false, true)
    (a pw_=== b) shouldBe res
  }

  test("Vec pw_=!= (scalar)") {
    val a = vec[Rational](0, 1, 2, 2)
    val res = vec[Boolean](true, true, false, false)
    (a pw_=!= Rational(2)) shouldBe res
  }

  test("Vec pw_=!= (vec)") {
    val a = vec[Rational](0, 1, 2, 2)
    val b = vec[Rational](0, 2, 1, 2)
    val res = vec[Boolean](false, true, true, false)
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
