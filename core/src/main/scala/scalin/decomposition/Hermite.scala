package scalin.decomposition

import java.security.Policy

import spire.algebra._
import spire.syntax.field._
import spire.syntax.signed._
import spire.syntax.cfor._

import scala.annotation.tailrec
import scalin.syntax.all._
import spire.math.Polynomial

import scala.reflect.ClassTag

/** Hermite decomposition / Hermite normal form (column version)
  *
  * The matrices U and H are such that A*U = H, where the matrix
  * U is an unimodular transformation matrix and H is the result of the transformation,
  * i.e. H is in Hermite normal form.
  */
trait Hermite[A] {

  def U: scalin.immutable.Mat[A]
  def A: scalin.immutable.Mat[A]
  def H: scalin.immutable.Mat[A]

}

/** Reduces a commutative ring element up to multiplication by a ring unit.
  *
  * Let ~ be the equivalence relation such that r ~ s if there is a unit u
  * such that r = u s. We say that r and s are associates.
  *
  * Then, for a ring element `a`, this typeclass finds a canonical representative
  * in the associates of `a`.
  */
trait Associates[A] {
  def isCanonical(a: A): Boolean
  def canonical(a: A)(implicit A: CRing[A]): A = correction(a) * a

  /** Returns the correction unit u such that `u * a` is canonical. */
  def correction(a: A): A
}

object Associates {

  def apply[A](implicit ev: Associates[A]): Associates[A] = ev

  /** In a signed ring, the canonical representative is nonnegative. */
  implicit def fromSignedRing[A:CRing:Signed]: Associates[A] = new Associates[A] {
    def isCanonical(a: A): Boolean = a.isSignNonNegative
    def correction(a: A): A = if (a.isSignNegative) -CRing[A].one else CRing[A].one
  }

  /** For univariate polynomials over a field, the canonical representative is
    * a monic polynomial. */
  implicit def fromPolynomialRing[A:ClassTag:Eq:Field]: Associates[Polynomial[A]] = new Associates[Polynomial[A]] {
    def isCanonical(a: Polynomial[A]): Boolean = a.isZero || a.maxOrderTermCoeff.isOne
    def correction(a: Polynomial[A]): Polynomial[A] =
      Polynomial.constant(
        if (a.isZero) Field[A].one
        else a.maxOrderTermCoeff.reciprocal
      )
  }

}

object Hermite {

  case class Impl[A](A: scalin.immutable.Mat[A], H: scalin.immutable.Mat[A], U: scalin.immutable.Mat[A]) extends Hermite[A]

  @tailrec def isRowZero[A:Eq:AdditiveMonoid](A: scalin.immutable.Mat[A], row: Int, startCol: Int = 0): Boolean =
    if (startCol == A.nCols) true
    else if (!A(row, startCol).isZero) false
    else isRowZero(A, row, startCol + 1)

  // Implementation taken from Python project abelian under GPL3 license
  // https://pypi.org/project/abelian/
  // Original source https://github.com/tommyod/abelian/blob/master/abelian/linalg/factorizations.py
  def apply[A:Eq:EuclideanRing:Associates](A: scalin.immutable.Mat[A]): Hermite[A] = {
    import scalin.mutable.dense._
    val m = A.nRows
    val n = A.nCols
    val H = A.to[scalin.mutable.DenseMat[A]]
    val U = scalin.mutable.Mat.eye[A](n)
    var i = 0
    var j = 0
    // Iterate down the rows of the matrix
    while (i < m && j < n) {
      // If every entry to the right is a zero, no pivot will be found for this row
      if (!isRowZero(A, i, j)) {
        // Create zeros to the right of the pivot H(i, j)
        cforRange(j + 1 until n) { k =>
          // Skip the column if the element is zero
          // In this case the column index j is not incremented
          if (!H(i, k).isZero) {
            // Apply the 'elementary hermite transform' to the columns of H,
            // ignoring the top i rows of H as they are identically zero
            // The implementation of the 'elementary hermite transform'
            // does not explicitly compute matrix products.
            // Equivalent to right-multiplication by the matrix [a, -s/g]
            //                                                  [b, r/g ]
            val r = H(i, j)
            val s = H(i, k)
            // Extended Euclidean algorithm, so that r*a + s*b = g = gcd(r, s)
            val (g, a, b) = GCDex(r, s)
            val rq = r equot g
            val sq = s equot g
            cforRange(i until m) { l =>
              val Hlj = H(l, j)
              val Hlk = H(l, k)
              H(l, j) := a * Hlj + b * Hlk
              H(l, k) := rq * Hlk - sq * Hlj
            }
            cforRange(0 until n) { l =>
              val Ulj = U(l, j)
              val Ulk = U(l, k)
              U(l, j) := a * Ulj + b * Ulk
              U(l, k) := rq * Ulk - sq * Ulj
            }
          }
        }

        // Make sure the pivot element is canonical among its associates
        // Some saving achieved by realizing that the first i rows of H are
        // identically zero -- thus no multiplication is needed.
        val u = Associates[A].correction(H(i, j))
        if (!u.isOne) {
          cforRange(i until m) { l =>
            H(l, j) := u * H(l, j)
          }
          cforRange(0 until n) { l =>
            U(l, j) := u * U(l, j)
          }
        }

        // Making all elements to the left of the pivot H[i, j] smaller
        // than the pivot and positive using division algorithm transform
        cforRange(0 until j) { k =>
          // Compute quotient in the division algorithm, subtracting
          // the quotient times H[:, j] leaves a positive remainder
          val a = H(i, k) equot H(i, j)
          cforRange(0 until m) { l =>
            H(l, k) := H(l, k) - a * H(l, j)
          }
          cforRange(0 until n) { l =>
            U(l, k) := U(l, k) - a * U(l, j)
          }
        }
        i += 1
        j += 1
      }
    }
    import scalin.immutable.dense._
    Impl(A, scalin.immutable.Mat.fromMutable(H)(x => () ), scalin.immutable.Mat.fromMutable(U)(x => () ))
  }

}

object GCDex {

  /** Returns (h, s, t) such that h = gcd(f, g) and s*f + t*g = h */
  def apply[A:Eq:EuclideanRing](f: A, g: A): (A, A, A) = {
    @tailrec def iter(a: A, b: A, aa0: A, bb0: A, aa1: A, bb1: A): (A, A, A) =
    if (b.isZero) (a, aa0, bb0) else {
      val (q, r) = a equotmod b
      iter(b, r, aa1, bb1, aa0 - q * aa1, bb0 - q * bb1)
    }
    iter(f, g, EuclideanRing[A].one, EuclideanRing[A].zero, EuclideanRing[A].zero, EuclideanRing[A].one)
  }

}
