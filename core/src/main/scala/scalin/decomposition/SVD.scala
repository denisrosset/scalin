package scalin.decomposition

import spire.implicits._
import scalin.immutable.{Mat, MatEngine, Vec}
import scalin.syntax.all._
import spire.algebra.{Field, NRoot, Signed}

import scala.annotation.tailrec

object SVD {

  /** Returns the singular value decomposition of the given matrix
    *
    * @param A     Matrix to decompose
    * @param wantU Whether to compute the left singular vectors
    * @param wantV Whether to compute the right singular vectors
    */
  def apply[A: Epsilon : Field : Signed : MatEngine : NRoot](A: Mat[A], wantU: Boolean = true, wantV: Boolean = true): SVD[A] =
    if (A.nRows < A.nCols) Transposed(SVD(A.t, wantV, wantU)) else new SVDImpl[A](A, wantU, wantV)

  /** Expresses the transposition of a singular value decomposition. */
  case class Transposed[A](svd: SVD[A]) extends SVD[A] {
    def t: SVD[A] = svd
    def U: Mat[A] = svd.V
    def S: Mat[A] = svd.S
    def V: Mat[A] = svd.U
    def singularValues: Vec[A] = svd.singularValues
    def norm2: A = svd.norm2
    def cond: A = svd.cond
    def rank: Int = svd.rank
  }

}

trait SVD[A] {
  /** Transpose of this SVD */
  def t: SVD[A]
  /** Left singular vectors */
  def U: Mat[A]
  /** Singular value diagonal matrix */
  def S: Mat[A]
  /** Right singular vectors */
  def V: Mat[A]
  /** Vector of singular values */
  def singularValues: Vec[A]
  /** Matrix represented by this SVD decomposition */
  def value(implicit A: Field[A], ev: MatEngine[A]): Mat[A] = U * S * V.t
  /** 2-norm of the matrix, i.e. largest singular value */
  def norm2: A
  /** Condition number of the matrix, i.e. ratio of the largest vs smallest singular value */
  def cond: A
  /** Estimated rank of the matrix */
  def rank: Int
}

class SVDImpl[A:Epsilon:Field:Signed:NRoot](arg: Mat[A], wantU: Boolean = true, wantV: Boolean = true) extends SVD[A] {
  import scalin.mutable.dense._

  val eps = Epsilon[A].eps
  val tiny = Epsilon[A].tiny
  val A = arg.to[scalin.mutable.Mat[A]]
  /** Row dimension */
  val m = A.nRows
  /** Column dimension */
  val n = A.nCols

  require(m >= n, "SVD only works for m >= n")

  val nu: Int = spire.math.min(m, n)
  /** Array for internal storage of singular values. */
  private[this] val s = scalin.mutable.Vec.zeros[A](spire.math.min(m + 1, n))
  private[this] val _U = scalin.mutable.Mat.zeros[A](m, nu)
  private[this] val _V = scalin.mutable.Mat.zeros[A](n, n)
  private[this] val e = scalin.mutable.Vec.zeros[A](n)
  private[this] val work = scalin.mutable.Vec.zeros[A](m)

  // Reduce A to bidiagonal form, storing the diagonal elements
  // in s and the super-diagonal elements in e.

  private[this] val nct: Int = spire.math.min(m - 1, n)
  private[this] val nrt: Int = spire.math.max(0, spire.math.min(n - 2, m))

  cforRange(0 until spire.math.max(nct, nrt)) { k =>
    if (k < nct) {
      // Compute the transformation for the k-th column and
      // place the k-th diagonal in s[k].
      // Compute 2-norm of k-th column without under/overflow.
      s(k) := Field[A].zero
      cforRange(k until m) { i =>
        s(k) := spire.math.hypot(s(k), A(i, k))
      }
      if (!s(k).isZero) {
        if (A(k, k).isSignNegative) s(k) := -s(k)
        cforRange(k until m) { i =>
          A(i, k) := A(i, k) / s(k)
        }
        A(k, k) := A(k, k) + Field[A].one
      }
      s(k) := -s(k)
    }
    cforRange(k + 1 until n) { j =>
      if ((k < nct) && (!s(k).isZero)) {

        // Apply the transformation.

        var t = Field[A].zero
        cforRange(k until m) { i =>
          t += A(i, k) * A(i, j)
        }
        t = -t / A(k, k)
        cforRange(k until m) { i =>
          A(i, j) := A(i, j) + t * A(i, k)
        }
      }

      // Place the k-th row of A into e for the
      // subsequent calculation of the row transformation.

      e(j) := A(k, j)
    }
    if (wantU && (k < nct)) {

      // Place the transformation in U for subsequent back
      // multiplication.

      cforRange(k until m) { i =>
        _U(i, k) := A(i, k)
      }
    }
    if (k < nrt) {

      // Compute the k-th row transformation and place the
      // k-th super-diagonal in e[k].
      // Compute 2-norm without under/overflow.
      e(k) := Field[A].zero
      cforRange(k+1 until n) { i =>
        e(k) := spire.math.hypot(e(k), e(i))
      }
      if (!e(k).isZero) {
        if (e(k + 1).isSignNegative) e(k) := -e(k)
        cforRange(k+1 until n) { i =>
          e(i) := e(i) / e(k)
        }
        e(k + 1) := e(k + 1) + Field[A].one
      }
      e(k) := -e(k)
      if ((k + 1 < m) && (!e(k).isZero)) {

        // Apply the transformation.

        cforRange(k+1 until m) { i =>
          work(i) := Field[A].zero
        }
        cforRange(k+1 until n) { j =>
          cforRange(k + 1 until m) { i =>
            work(i) := work(i) + e(j) * A(i, j)
          }
        }
        cforRange(k+1 until n) { j =>
          val t = -e(j) / e(k + 1)
          cforRange(k + 1 until m) { i =>
            A(i, j) := A(i, j) + t * work(i)
          }
        }
      }
      if (wantV) {

        // Place the transformation in V for subsequent
        // back multiplication.

        cforRange(k+1 until n) { i =>
          _V(i, k) := e(i)
        }
      }
    }
  }

  // Set up the final bidiagonal matrix or order p.

  var p: Int = Math.min(n, m + 1)
  if (nct < n) s(nct) := A(nct, nct)
  if (m < p) s(p - 1) := Field[A].zero
  if (nrt + 1 < p) e(nrt) := A(nrt, p - 1)
  e(p - 1) := Field[A].zero

  // If required, generate U.

  if (wantU) {
    cforRange(nct until nu) { j =>
      cforRange(0 until m) { i =>
        _U(i, j) := Field[A].zero
      }
      _U(j, j) := Field[A].one
    }
    cforRange(nct - 1 to 0 by -1) { k =>
      if (!s(k).isZero) {
        cforRange(k + 1 until nu) { j =>
          var t = Field[A].zero
          cforRange(k until m) { i =>
            t += _U(i, k) * _U(i, j)
          }
          t = -t / _U(k, k)
          cforRange(k until m) { i =>
            _U(i, j) := _U(i, j) + t * _U(i, k)
          }
        }
        cforRange(k until m) { i =>
          _U(i, k) := -_U(i, k)
        }
        _U(k, k) := Field[A].one + _U(k, k)
        cforRange(0 until k - 1) { i =>
          _U(i, k) := Field[A].zero
        }
      } else {
        cforRange(0 until m) { i =>
          _U(i, k) := Field[A].zero
        }
        _U(k, k) := Field[A].one
      }
    }
  }

  // If required, generate V.

  if (wantV) {
    cforRange(n - 1 to 0 by -1) { k =>
      if ((k < nrt) && (!e(k).isZero)) {
        cforRange(k + 1 until nu) { j =>
          var t = Field[A].zero
          cforRange(k + 1 until n) { i =>
            t += _V(i, k) * _V(i, j)
          }
          t = -t / _V(k + 1, k)
          cforRange(k + 1 until n) { i =>
            _V(i, j) := _V(i, j) + t * _V(i, k)
          }
        }
      }
      cforRange(0 until n) { i =>
        _V(i, k) := Field[A].zero
      }
      _V(k, k) := Field[A].one
    }
  }

  // Main iteration loop for the singular values.

  val pp = p - 1
  var iter = 0

  while (p > 0) {
    // Here is where a test for too many iterations would go.
    // This section of the program inspects for
    // negligible elements in the s and e arrays.  On
    // completion the variables kase and k are set as follows.
    // kase = 1     if s(p) and e[k-1] are negligible and k<p
    // kase = 2     if s(k) is negligible and k<p
    // kase = 3     if e[k-1] is negligible, k<p, and
    //              s(k), ..., s(p) are not negligible (qr step).
    // kase = 4     if e(p-1) is negligible (convergence).

    @tailrec def iterk(k: Int): Int =
      if (k <= -1) k // break
      else if (e(k).abs <= tiny + eps * (s(k).abs + s(k + 1).abs)) {
        e(k) := Field[A].zero
        k // break
      } else iterk(k - 1)

    var k = iterk(p - 2)

    @tailrec def iterks(ks: Int): Int =
      if (ks <= k) ks // break
      else {
        var t = Field[A].zero
        if (ks != p) t += e(ks).abs
        if (ks != k + 1) t += e(ks - 1).abs
        if (s(ks).abs <= tiny + eps * t) {
          s(ks) := Field[A].zero
          ks // break
        } else iterks(ks - 1)
      }

    val kase = if (k == p - 2) 4 else {
      val ks = iterks(p - 1)
      if (ks == k) 3
      else if (ks == p - 1) 1
      else {
        k = ks
        2
      }
    }

    k += 1

    kase match {

      case 1 => // Deflate negligible s(p).

        var f = e(p - 2)
        e(p - 2) := Field[A].zero
        cforRange(p - 2 to k by -1) { j =>
          var t = spire.math.hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) := t
          if (j != k) {
            f = -sn * e(j - 1)
            e(j - 1) := cs * e(j - 1)
          }
          if (wantV) {
            cforRange(0 until n) { i =>
              t = cs * _V(i, j) + sn * _V(i, p - 1)
              _V(i, p - 1) := -sn * _V(i, j) + cs * _V(i, p - 1)
              _V(i, j) := t
            }
          }
        }
      case 2 => // Split at negligible s(k).
        var f = e(k - 1)
        e(k - 1) := Field[A].zero
        cforRange(k until p) { j =>
          var t = spire.math.hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) := t
          f = -sn * e(j)
          e(j) := cs * e(j)
          if (wantU) {
            cforRange(0 until m) { i =>
              t = cs * _U(i, j) + sn * _U(i, k - 1)
              _U(i, k - 1) := -sn * _U(i, j) + cs * _U(i, k - 1)
              _U(i, j) := t
            }
          }
        }

      case 3 => // Perform one qr step.

        // Calculate the shift.
        import spire.math.max
        val scale = max(max(
          max(s(p - 1).abs, s(p - 2).abs),
          max(e(p - 2).abs, s(k).abs)
        ), e(k).abs)
        val sp = s(p - 1) / scale
        val spm1 = s(p - 2) / scale
        val epm1 = e(p - 2) / scale
        val sk = s(k) / scale
        val ek = e(k) / scale
        val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
        val c = (sp * epm1) * (sp * epm1)
        var shift = Field[A].zero
        if (!b.isZero || !c.isZero) {
          shift = spire.math.sqrt(b * b + c)
          if (b.isSignNegative) shift = -shift
          shift = c / (b + shift)
        }
        var f = (sk + sp) * (sk - sp) + shift
        var g = sk * ek

        // Chase zeros.

        cforRange(k until p - 1) { j =>
          var t = spire.math.hypot(f, g)
          var cs = f / t
          var sn = g / t
          if (j != k) e(j - 1) := t
          f = cs * s(j) + sn * e(j)
          e(j) := cs * e(j) - sn * s(j)
          g = sn * s(j + 1)
          s(j + 1) := cs * s(j + 1)
          if (wantV) {
            cforRange(0 until n) { i =>
              t = cs * _V(i, j) + sn * _V(i, j + 1)
              _V(i, j + 1) := -sn * _V(i, j) + cs * _V(i, j + 1)
              _V(i, j) := t
            }
          }
          t = spire.math.hypot(f, g)
          cs = f / t
          sn = g / t
          s(j) := t
          f = cs * e(j) + sn * s(j + 1)
          s(j + 1) := -sn * e(j) + cs * s(j + 1)
          g = sn * e(j + 1)
          e(j + 1) := cs * e(j + 1)
          if (wantU && (j < m - 1)) {
            cforRange(0 until m) { i =>
              t = cs * _U(i, j) + sn * _U(i, j + 1)
              _U(i, j + 1) := -sn * _U(i, j) + cs * _U(i, j + 1)
              _U(i, j) := t
            }
          }
        }
        e(p - 2) := f
        iter = iter + 1

      case 4 => // Convergence.

        // Make the singular values positive.

        if (s(k).isSignNonPositive()) {
          s(k) := (if (s(k).isSignNegative) -s(k) else Field[A].zero)
          if (wantV) {
            cforRange(0 to pp) { i =>
              _V(i, k) := -_V(i, k)
            }
          }
        }

        // Order the singular values.

        while (k < pp && s(k) < s(k+1)) {
          var t = s(k)
          s(k) := s(k + 1)
          s(k + 1) := t
          if (wantV && (k < n - 1)) {
            cforRange(0 until n) { i =>
              t = _V(i, k + 1)
              _V(i, k + 1) := _V(i, k)
              _V(i, k) := t
            }
          }
          if (wantU && (k < m - 1)) {
            cforRange(0 until m) { i =>
              t = _U(i, k + 1)
              _U(i, k + 1) := _U(i, k)
              _U(i, k) := t
            }
          }
          k += 1
        }
        iter = 0
        p -= 1
    }

  }

  def t: SVD[A] = SVD.Transposed(this)

  lazy val U: Mat[A] = {
    import scalin.immutable.dense._
    Mat.tabulate(m, spire.math.min(m + 1, n))( (r, c) => _U(r, c) )
  }

  lazy val V: Mat[A] = {
    import scalin.immutable.dense._
    Mat.tabulate(n, n)( (r, c) => _V(r, c) )
  }

  lazy val singularValues: Vec[A] = {
    import scalin.immutable.dense._
    s.to[Vec[A]]
  }

  lazy val S: Mat[A] = {
    import scalin.immutable.dense._
    singularValues.toDiagMat[Mat[A]]
  }

  def norm2: A = s(0)

  def cond: A = s(0)/s(spire.math.min(m, n) - 1)

  def rank: Int = {
    val tol = spire.math.max(m, n) * s(0) * eps
    var r = 0
    cforRange(0 until s.length) { i =>
      if (s(i) > tol) r += 1
    }
    r
  }

}
