package scalin

import scalin.mutable.dense._

class KroneckerSuite extends ScalinSuite {

  test("Wikipedia example: kronecker product of matrices") {
    val lhs = Mat.rowMajor[Int](2,2)(
      1,2,
      3,4)
    val rhs = Mat.rowMajor[Int](2,2)(
      0,5,
      6,7)
    (lhs kron rhs) shouldBe Mat.rowMajor(4,4)(
      0,5,0,10,
      6,7,12,14,
      0,15,0,20,
      18,21,24,28)
  }

  test("Example") {
    val lhs = Vec[Int](2,3,4)
    val rhs = Vec[Int](0,1)
    (lhs kron rhs) shouldBe Vec(0,2,0,3,0,4)
  }


}
