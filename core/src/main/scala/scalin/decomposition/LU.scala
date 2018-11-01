package scalin.decomposition

import scalin.Pivot
import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._
import spire.algebra.Field
import spire.syntax.cfor._
import spire.syntax.field._

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
trait LU[A] { lhs =>

  def P: Mat[A]
  def L: Mat[A]
  def U: Mat[A]

  def value(implicit A: Field[A], ev: scalin.immutable.MatEngine[A]): Mat[A] = P * L * U

  def nPivots: Int
  def pivot(k: Int): Int
  def pivots: Vector[Int]
  def permutationCount: Int

  def determinant: A

  def isSingular: Boolean

  def inverse: Mat[A]

  def solve(rhs: Mat[A]): Mat[A]
  def solve(rhs: Vec[A]): Vec[A]

}

final class LUImpl[A:Field:Pivot](val _pivots: Array[Int],
                                  val permutationCount: Int,
                                  lu: scalin.Mat[A]) extends LU[A] {
  import scalin.immutable.dense._

  def nPivots = _pivots.length

  def pivot(k: Int) = _pivots(k)

  def pivots: Vector[Int] = _pivots.toVector

  val m = lu.nRows // row dimension
  val n = lu.nCols // column dimension

  lazy val pivotsInverse = {
    val res = new Array[Int](_pivots.length)
    cforRange(0 until _pivots.length) { i =>
      res(_pivots(i)) = i
    }
    res
  }

  def P: Mat[A] = Mat.fromMutable[A](n, n, Field[A].zero) { mut =>
    cforRange(0 until n) { i =>
      mut(_pivots(i), i) := Field[A].one
    }
  }

  def L: Mat[A] = Mat.fromMutable[A](m, n, Field[A].zero) { mut =>
    cforRange(0 until n) { i => mut(i, i) := Field[A].one }
    cforRange(0 until m) { i =>
      cforRange(0 until i) { j =>
        mut(i, j) := lu(i, j)
      }
    }
  }

  def U: Mat[A] = Mat.fromMutable[A](n, n, Field[A].zero) { mut =>
    cforRange(0 until n) { i =>
      cforRange(i until n) { j =>
        mut(i, j) := lu(i, j)
      }
    }
  }

  def isSingular: Boolean = {
    cforRange(0 until n) { j =>
      if (Pivot[A].closeToZero(lu(j, j))) return true
    }
    false
  }

  def determinant: A = {
    if (m != n)
      throw new IllegalArgumentException("Matrix must be square.")
    var d = if ((permutationCount & 1) == 0) Field[A].one else -Field[A].one
    cforRange(0 until n) { j =>
      d *= lu(j, j)
    }
    d
  }

  def solve(b: Vec[A]): Vec[A] = {
    require(lu.nCols <= lu.nRows)
    if (b.length != m)
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (isSingular)
      throw new RuntimeException("Matrix is singular.")
    // Copy right hand side with pivoting
    val res = Vec.fromMutable[A](_pivots.length, Field[A].zero) { x =>
      cforRange(0 until _pivots.length) { i =>
        x(i) := b(_pivots(i))
      }
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
    }
    if (res.length > lu.nCols) res(0 until lu.nCols) else res
  }

  def solve(b: Mat[A]): Mat[A] = {
    require(lu.nCols <= lu.nRows)
    if (b.nRows != m)
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (isSingular)
      throw new RuntimeException("Matrix is singular.")
    val nx = b.nCols
    val res = Mat.fromMutable[A](_pivots.length, b.nCols, Field[A].zero) { x =>
      // Copy right hand side with pivoting
      cforRange(0 until _pivots.length) { r =>
        cforRange(0 until b.nCols) { c =>
          x(r, c) := b(_pivots(r), c)
        }
      }

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
    }
    if (res.nRows > lu.nCols) res(0 until lu.nCols, ::) else res
  }

  def inverse: Mat[A] = Mat.fromMutable[A](n, n, Field[A].zero) { r =>
    require(m == n)
    // fill with upper
    cforRange(0 until n) { i =>
      cforRange(i until n) { j =>
        r(i, j) := lu(i, j)
      }
    }
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
    scalin.computation.Permute.colsPermuteInverse[A, scalin.mutable.Mat[A]](r, pivotsInverse)
  }
}

object LU {

  def inverse[A:Field:Pivot](lhs: scalin.Mat[A]): Mat[A] = {
    import scalin.mutable.dense._
    inPlaceLU(lhs.to[scalin.mutable.DenseMat[A]]).inverse
  }

  def determinant[A:Field:Pivot](lhs: scalin.Mat[A]): A = {
    import scalin.mutable.dense._
    inPlaceLU(lhs.to[scalin.mutable.DenseMat[A]]).determinant
  }

  def apply[A:Field:Pivot](mat: Mat[A]): LU[A] = {
    import scalin.mutable.dense._
    inPlaceLU(mat.to[scalin.mutable.Mat[A]])
  }
  /** Implementation taken from the JAMA library (NIST), in the public domain, and
    * translated to Scala.
    */
  def inPlaceLU[A:Field:Pivot](lu: scalin.mutable.Mat[A]): LU[A] = {
    val m = lu.nRows // row dimension
    val n = lu.nCols // column dimension
    require(m >= n)
    val piv = Array.tabulate(m)(identity) // internal storage of pivot vector
    var pCount = 0 // permutation count
                   // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

    // Outer loop.
    val luColj = scalin.mutable.Vec.zeros[A](m)

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
        scalin.computation.Permute.rowsPermute[A, scalin.mutable.Mat[A]](lu, p, j)
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
    new LUImpl(piv, pCount, lu)
  }
}
