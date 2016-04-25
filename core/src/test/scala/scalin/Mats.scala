package scalin

import spire.algebra.{Eq, Ring}
import spire.syntax.cfor._
import spire.syntax.ring._

import scalin.syntax.all._
import org.scalacheck.{Arbitrary, Gen}

object Mats {

  /** Generates an upper-triangular matrix with non-zero elements on the diagonal. */
  def genUpperDiagNZ[A:Arbitrary:Eq:Ring](n: Int): Gen[Mat[A]] = {
    import scalin.mutable.dense._
    val nonZero = Arbitrary.arbitrary[A].filter(!_.isZero)
    for {
      values <- Gen.containerOfN[IndexedSeq, A]((n - 1) * (n - 2) / 2, Arbitrary.arbitrary[A])
      diag <- Gen.containerOfN[IndexedSeq, A](n, nonZero)
    } yield {
      val res = zeros[A](n, n)
      cforRange(0 until n) { k =>
        res(k, k) := diag(k)
      }
      var i = 0
      cforRange(1 until n) { r =>
        cforRange(r + 1 until n) { c =>
          res(r, c) := values(i)
          i += 1
        }
      }
      res.result()
    }
  }

  /** Generates an upper-triangular matrix with ones on the diagonal. */
  def genUpperDiagOne[A:Arbitrary:Ring](n: Int): Gen[Mat[A]] = {
    import scalin.mutable.dense._
    Gen.containerOfN[IndexedSeq, A]((n - 1) * (n - 2) / 2, Arbitrary.arbitrary[A]).map { values =>
      val res = eye[A](n)
      var i = 0
      cforRange(1 until n) { r =>
        cforRange(r + 1 until n) { c =>
          res(r, c) := values(i)
          i += 1
        }
      }
      res.result()
    }
  }

  /** Generates a matrix of determinant equal to one. */
  def genDetOne[A:Arbitrary:Ring](n: Int): Gen[Mat[A]] = {
    import scalin.immutable.dense._
    for {
      lhs <- genUpperDiagOne(n)
      rhs <- genUpperDiagOne(n)
    } yield lhs * rhs.t
  }

  /** Generates a full rank matrix. */
  def genFullRank[A:Arbitrary:Eq:Ring](n: Int): Gen[Mat[A]] = {
    import scalin.immutable.dense._
    for {
      lhs <- genUpperDiagNZ(n)
      rhs <- genUpperDiagNZ(n)
    } yield lhs * rhs.t
  }


}
