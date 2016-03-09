package scalin

import scalin.mutable.dense._
import scalin.syntax.AllSyntax

import spire.algebra.Eq
import spire.math._
import spire.implicits._

import org.scalatest.{FunSuite, PropSpec, Matchers}

class RankFactorizationSuite extends FunSuite with Matchers with AllSyntax {

  test("Wikipedia example: rank factorization") {
    val A = rowMajor[Rational](4,4)(
      1,3,1,4,
      2,7,3,9,
      1,5,3,1,
      1,2,0,8)
    val fact = A.rankFactorization
    (fact.matC(A) * fact.matF) shouldBe A
  }

}
