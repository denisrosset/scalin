package scalin

import scalin.mutable.dense._
import scalin.syntax.AllSyntax

import spire.algebra.Eq
import spire.math._
import spire.implicits._

import org.scalatest.{FunSuite, PropSpec, Matchers}

class KroneckerSuite extends FunSuite with Matchers with AllSyntax {

  test("Wikipedia example: kronecker product of matrices") {
    val lhs = rowMajor[Int](2,2)(
      1,2,
      3,4)
    val rhs = rowMajor[Int](2,2)(
      0,5,
      6,7)
    (lhs kron rhs) shouldBe rowMajor(4,4)(
      0,5,0,10,
      6,7,12,14,
      0,15,0,20,
      18,21,24,28)
  }

  test("Example") {
    val lhs = vec[Int](2,3,4)
    val rhs = vec[Int](0,1)
    (lhs kron rhs) shouldBe vec(0,2,0,3,0,4)
  }


}
