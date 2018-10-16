package scalin.decomposition

import scalin.Sparse
import scalin.immutable.Mat
import scalin.syntax.all._
import spire.algebra.{Field, Signed}
import spire.implicits._

/** LDL Cholesky decomposition. Is an exact decomposition that only requires
  * field operations, and can thus be performed on the rational numbers.
  *
  * The original matrix is = L * D * L.t
  */
class LDL[F](L: Mat[F], D: Mat[F]) {

  def value(implicit ev: Field[F]): Mat[F] = {
    import scalin.immutable.dense._
    L * D * L.t
  }

}

object LDL {

  def apply[F:Field:Signed:Sparse](A: Mat[F]): Option[LDL[F]] = {
    // from the algorithm on Wikipedia Cholesky page, we added the Macaulay2 LDL tests to cater
    // for semidefinite positive matrices
    val d = A.nRows
    val L = scalin.mutable.DenseMat.defaultEngine[F].zeros(d, d)
    val D = scalin.mutable.CSCMat.defaultEngine[F].zeros(d, d)
    val p = Array.tabulate(d)(identity)
    cforRange(0 until d) { j =>
      var maxInd = j
      var maxVal = A(p(j), p(j))
      cforRange(j + 1 until d) { k =>
        val test = A(p(k), p(k))
        if (test > maxVal) {
          maxInd = k
          maxVal = test
        }
      }
      // swap
      val tmpSwap = p(maxInd)
      p(maxInd) = p(j)
      p(j) = tmpSwap
      // test SDP condition
      if (maxVal.isSignNegative) return None
      if (maxVal.isSignZero) {
        cforRange(0 until d) { i =>
          if (A(p(i),p(j)).isSignNonZero) return None
        }
      }
      if (maxVal.isSignPositive) {
        L(p(j), p(j)) := Field[F].one
        var tmp = A(p(j), p(j))
        cforRange(0 until j) { k =>
          tmp = tmp - L(p(j), p(k)) * L(p(j), p(k)) * D(p(k), p(k))
        }
        D(p(j), p(j)) := tmp
        if (tmp.isSignNegative) return None
        if (tmp.isSignPositive) {
          cforRange(j + 1 until d) { i =>
            tmp = A(p(i), p(j))
            cforRange(0 until j) { k =>
              tmp = tmp - L(p(i), p(k)) * L(p(j), p(k)) * D(p(k), p(k))
            }
            L(p(i), p(j)) := tmp / D(p(j), p(j))
          }
        }
      }
    }
    val iL = L.to[scalin.immutable.DenseMat[F]]
    val iD = D.to[scalin.immutable.CSCMat[F]]
    Some(new LDL(iL, iD))
  }

}
