package scalin
package algos

import spire.algebra.Field
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.syntax.assign._

/** LU Decomposition.
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * 
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the construction will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations. This will fail if isSingular returns true.
  */
trait LUDecomposition[A] { lhs =>

  type MA <: Mat[A]
  type VA <: Vec[A]

  def nPivots: Int
  def pivot(k: Int): Int

  def permutationCount: Int
  def permutation: MA

  def determinant: A

  def lower: MA
  def upper: MA
  def isSingular: Boolean

  def inverse: MA

  def solve(rhs: Mat[A]): MA
  def solve(rhs: Vec[A]): VA

}

final class LUDecompositionImpl[A](
  val pivots: Array[Int],
  val permutationCount: Int,
  lu: mutable.Mat[A])(implicit A: Field[A], MA: mutable.MatEngine[A], VA: mutable.VecEngine[A], pivotA: Pivot[A]) extends LUDecomposition[A] {

  def nPivots = pivots.length

  def pivot(k: Int) = pivots(k)

  type MA = mutable.Mat[A]
  type VA = mutable.Vec[A]

  val m = lu.nRows // row dimension
  val n = lu.nCols // column dimension

  lazy val pivotsInverse = {
    val res = new Array[Int](pivots.length)
    cforRange(0 until pivots.length) { i =>
      res(pivots(i)) = i
    }
    res
  }

  def permutation: MA = {
    val p = MA.zeros(n, n)
    cforRange(0 until n) { i => p(pivots(i), i) := A.one }
    p
  }

  def lower: MA = {
    val l = MA.zeros(m, n)
    cforRange(0 until n) { i => l(i, i) := A.one }
    cforRange(0 until m) { i =>
      cforRange(0 until i) { j =>
        l(i, j) := lu(i, j)
      }
    }
    l
  }

  def upper: MA = {
    val u = MA.zeros(n, n)
    cforRange(0 until n) { i =>
      cforRange(i until n) { j =>
        u(i, j) := lu(i, j)
      }
    }
    u
  }

  def isSingular: Boolean = {
    cforRange(0 until n) { j =>
      if (pivotA.closeToZero(lu(j, j))) return true
    }
    false
  }

  def determinant: A = {
    if (m != n)
      throw new IllegalArgumentException("Matrix must be square.")
    var d = if ((permutationCount & 1) == 0) A.one else -A.one
    cforRange(0 until n) { j =>
      d *= lu(j, j)
    }
    d
  }

  def solve(b: Vec[A]): VA = {
    require(lu.nCols <= lu.nRows)
    if (b.length != m)
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (isSingular)
      throw new RuntimeException("Matrix is singular.")
    // Copy right hand side with pivoting
    val x = VA.slice(b, pivots)
    // Solve L*Y = B(piv,:)
    cforRange(0 until n) { k =>
      cforRange(k + 1 until n) { i =>
        x(i) := x(i) - x(k) * lu(i, k)
      }
    }

    // Solve U*X = Y;
    cforRange(n - 1 to 0 by -1) { k =>
      x(k) := x(k) / lu(k, k)
      cforRange(0 until k) { i =>
        x(i) := x(i) - x(k) * lu(i, k)
      }
    }
    if (x.length > lu.nCols) VA.slice(x, 0 until lu.nCols) else x
  }

  def solve(b: Mat[A]): MA = {
    require(lu.nCols <= lu.nRows)
    if (b.nRows != m)
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (isSingular)
      throw new RuntimeException("Matrix is singular.")
    val nx = b.nCols
    // Copy right hand side with pivoting
    val x = MA.slice(b, pivots, ::)

    // Solve L*Y = B(piv,:)
    cforRange(0 until n) { k =>
      cforRange(k + 1 until n) { i =>
        cforRange(0 until nx) { j =>
          x(i, j) := x(i, j) - x(k, j) * lu(i, k)
        }
      }
    }

    // Solve U*X = Y;
    cforRange(n - 1 to 0 by -1) { k =>
      cforRange(0 until nx) { j =>
        x(k, j) := x(k, j) / lu(k, k)
      }
      cforRange(0 until k) { i =>
        cforRange(0 until nx) { j =>
          x(i, j) := x(i, j) - x(k, j) * lu(i, k)
        }
      }
    }
    if (x.nRows > lu.nCols) MA.slice(x, 0 until lu.nCols, ::) else x
  }

  def inverse: MA = {
    require(m == n)
    val r = upper
    // Calculate inv(upper)
    cforRange(n - 1 to 0 by -1) { j =>
      r(j, j) := r(j, j).reciprocal
      cforRange(j - 1 to 0 by -1) { i =>
        var sum = -r(i, j) * r(j, j)
        cforRange(j - 1 until i by - 1) { k =>
          sum = sum - r(i, k) * r(k, j)
        }
        r(i, j) := sum / r(i, i)
      }
    }
    // Solve inv(I) * lower = inv(upper)
    cforRange(0 until n) { i =>
      cforRange(n - 2 to 0 by -1) { j =>
        cforRange(j + 1 until n) { k =>
          r(i, j) := r(i, j) - r(i, k) * lu(k, j)
        }
      }
    }
    // Correct pivot permutations.
    algos.Permute.colsPermuteInverse[A, MA](r, pivotsInverse)
    r
  }
}

object LUDecomposition {

  def inverse[A:Field:Pivot](lhs: Mat[A])(implicit MA: mutable.MatEngine[A], VA: mutable.VecEngine[A]): mutable.Mat[A] = inPlaceLU(MA.fromMat(lhs)).inverse

  def determinant[A:Field:Pivot](lhs: Mat[A])(implicit MA: mutable.MatEngine[A], VA: mutable.VecEngine[A]): A = inPlaceLU(MA.fromMat(lhs)).determinant

  /** Implementation taken from the JAMA library (NIST), in the public domain, and
    * translated to Scala.
    */
  def inPlaceLU[A:Field:Pivot](lu: mutable.Mat[A])(implicit MA: mutable.MatEngine[A], VA: mutable.VecEngine[A]): LUDecomposition[A] { type MA = mutable.Mat[A]; type VA = mutable.Vec[A] } = {
    val m = lu.nRows // row dimension
    val n = lu.nCols // column dimension
    require(m >= n)
    val piv = Array.tabulate(m)(identity) // internal storage of pivot vector
    var pCount = 0 // permutation count
                   // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

    // Outer loop.
    val luColj = VA.zeros(m)

    cforRange(0 until n) { j =>
      // Make a copy of the j-th column to localize references.
      cforRange(0 until m) { i => luColj(i) := lu(i, j) }

      // Apply previous transformations.
      cforRange(0 until m) { i =>
        // Most of the time is spent in the following dot product.
        val kmax = spire.math.min(i, j)
        var s = Field[A].zero
        cforRange(0 until kmax) { k => // TODO: replace by vector operation when optimized
          s = s + lu(i, k) * luColj(k)
        }
        val nv = luColj(i) - s
        luColj(i) := nv
        lu(i, j) := nv
      }
      // Find pivot and exchange if necessary.
      var p = j
      var pPriority = Pivot[A].priority(luColj(j))
      cforRange(j + 1 until m) { i =>
        val iPriority = Pivot[A].priority(luColj(i))
        if (iPriority > pPriority) {
          p = i
          pPriority = iPriority
        }
      }
      if (p != j) {
        algos.Permute.rowsPermute[A, mutable.Mat[A]](lu, p, j)
        val t = piv(p)
        piv(p) = piv(j)
        piv(j) = t
        pCount += 1
      }

      // Compute multipliers.
      val diag = lu(j, j)
      if (j < m && !Pivot[A].closeToZero(diag)) {
        cforRange(j + 1 until m) { i =>
          lu(i, j) := lu(i, j) / diag
        }
      }
    }
    new LUDecompositionImpl(piv, pCount, lu)
  }

}
