package scalin.decomposition

import spire.algebra.{Field, NRoot, Signed}
import spire.syntax.cfor._
import spire.syntax.field._
import spire.syntax.signed._
import scalin.syntax.all._

/** QR Decomposition
  *
  * For an m-by-n matrix A with m >= n, the QR decomposition is an m-by-n
  * orthogonal matrix Q and an n-by-n upper triangular matrix R so that
  * A = Q*R.
  */
trait QR[A] {
  /** Number of rows of A */
  def m: Int

  /** Number of columns of A */
  def n: Int

  /** Whether A is full rank */
  def isFullRank: Boolean

  /** Orthogonal factor (economy-sized) */
  def Q: scalin.immutable.Mat[A]

  /** Upper triangular factor */
  def R: scalin.immutable.Mat[A]

  def value(implicit A: Field[A], ev: scalin.immutable.MatEngine[A]): scalin.immutable.Mat[A] = Q * R

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @exception IllegalArgumentException  Matrix row dimensions must agree.
    * @exception RuntimeException          Matrix is rank deficient.
    */
  def solve(B: scalin.immutable.Mat[A]): scalin.immutable.Mat[A]
}

object QR {

  def apply[A:Field:NRoot:Signed](A: scalin.immutable.Mat[A]): QR[A] = new QRImpl[A](A)

}

/** QR Decomposition, converted to Scala generic code from the Jama library
  * https://math.nist.gov/javanumerics/jama/
  *
  * For an m-by-n matrix A with m >= n, the QR decomposition is an m-by-n
  * orthogonal matrix Q and an n-by-n upper triangular matrix R so that
  * A = Q*R.
  *
  * The QR decompostion always exists, even if the matrix does not have
  * full rank, so the constructor will never fail.  The primary use of the
  * QR decomposition is in the least squares solution of nonsquare systems
  * of simultaneous linear equations.  This will fail if isFullRank()
  * returns false.
  */
class QRImpl[A:Field:NRoot:Signed](val A: scalin.immutable.Mat[A]) extends QR[A] {

  /** Internal storage of decomposition. */
  private[this] val QR = {
    import scalin.mutable.dense._
    A.to[scalin.mutable.Mat[A]]
  }
  /** Row dimension */
  val m: Int = A.nRows
  /** Column dimension */
  val n: Int = A.nCols
  /** Internal storage of diagonal of R. */
  private[this] val Rdiag = {
    import scalin.mutable.dense._
    scalin.mutable.Vec.zeros[A](n)
  }

  mainLoop()

  def mainLoop(): Unit = {
    // Main loop.
    cforRange(0 until n) { k =>
      // Compute 2-norm of k-th column without under/overflow.
      var nrm = Field[A].zero
      cforRange(k until m) { i =>
        nrm = spire.math.hypot(nrm, QR(i, k))
      }
      if (!nrm.isZero) {
        // Form k-th Householder vector.
        if (QR(k, k).isSignNegative)
          nrm = -nrm
        cforRange(k until m) { i =>
          QR(i, k) := QR(i, k) / nrm
        }
        QR(k, k) := QR(k, k) + Field[A].one
        // Apply transformation to remaining columns.
        cforRange(k + 1 until n) { j =>
          var s = Field[A].zero
          cforRange(k until m) { i =>
            s += QR(i, k) * QR(i, j)
          }
          s = -s / QR(k, k)
          cforRange(k until m) { i =>
            QR(i, j) := QR(i, j) + s * QR(i, k)
          }
        }
      }
      Rdiag(k) := -nrm
    }
  }


  /** Is the matrix full rank ? */
  def isFullRank: Boolean = {
    cforRange(0 until n) { j =>
      if (Rdiag(j).isZero)
        return false
    }
    true
  }

  /** Upper triangular factor */
  def R: scalin.immutable.Mat[A] =
    scalin.immutable.Mat.tabulate[A](n ,n) { (i, j) =>
      if (i < j) QR(i, j)
      else if (i == j) Rdiag(i)
      else Field[A].zero
    }

  /** Orthogonal factor (economy-sized) */
  lazy val Q: scalin.immutable.Mat[A] =
    scalin.immutable.Mat.fromMutable[A](m, n, Field[A].zero) { res =>
      cforRange(n - 1 to 0 by -1) { k =>
        cforRange(0 until m) { i =>
          res(i, k) := Field[A].zero
        }
        res(k, k) := Field[A].one
        cforRange(k until n) { j =>
          if (!QR(k, k).isZero) {
            var s = Field[A].zero
            cforRange(k until m) { i =>
              s += QR(i, k) * res(i, j)
            }
            s = -s / QR(k, k)
            cforRange(k until m) { i =>
              res(i, j) := res(i, j) + s * QR(i, k)
            }
          }
        }
      }
    }

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @exception IllegalArgumentException  Matrix row dimensions must agree.
    * @exception RuntimeException  Matrix is rank deficient.
    */
  def solve(B: scalin.immutable.Mat[A]): scalin.immutable.Mat[A] = {
    if (B.nRows != m) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!isFullRank) throw new RuntimeException("Matrix is rank deficient.")
    // Copy right hand side
    scalin.immutable.Mat.fromMutable(B) { X =>
      val nx = B.nRows
      // Compute Y = transpose(Q)*B
      cforRange(0 until n) { k =>
        cforRange(0 until nx) { j =>
          var s = Field[A].zero
          cforRange(k until m) { i =>
            s += QR(i, k) * X(i, j)
          }
          s = -s / QR(k, k)
          cforRange(k until m) { i =>
            X(i, j) := s * QR(i, k)
          }
        }
      }
      // Solve R*X = Y;
      cforRange(n - 1 to 0 by -1) { k =>
        cforRange(0 until nx) { j =>
          X(k, j) := X(k, j) / Rdiag(k)
          cforRange(0 until k) { i =>
            cforRange(0 until nx) { j =>
              X(i, j) := X(i, j) - X(k, j) * QR(i, k)
            }
          }
        }
      }
    }
  }

}