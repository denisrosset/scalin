package scalin
package impl
package builder

import spire.algebra.NRoot
import spire.math.{min, max}
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.algebra.Pivot
import scalin.syntax.assign._

trait MatField[A, MA <: Mat[A]]
    extends scalin.impl.MatField[A, MA]
    with scalin.impl.builder.MatEuclideanRing[A, MA] {

  implicit def UMA: scalin.algebra.MatField[A, UMA]
  implicit def UVA: scalin.algebra.VecField[A, UVA]

  def luDecomposition(lhs: Mat[A]): LUDecomposition[A] { type MA = UMA } = inplaceLU(UMA.fromMat(lhs))

  def rankFactorization(lhs: Mat[A]): RankFactorization[A] { type MA = UMA } = inplaceRankFactorization(UMA.fromMat(lhs))

  def inverse(lhs: Mat[A]): MA = result(luDecomposition(lhs).inverse)

  override def determinant(lhs: Mat[A]): A = luDecomposition(lhs).determinant

  override protected def orthogonalize(m: UMA): Unit = {
    import pivotA.closeToZero
    val nR = m.rows
    val nC = m.cols
    cforRange(0 until nR) { i =>
      cforRange(i + 1 until nR) { j =>
        var uv: A = scalar.zero
        var uu: A = scalar.zero
        cforRange(0 until nC) { c =>
          uv = uv + m(i, c) * m(j, c)
          uu = uu + m(i, c) * m(i, c)
        }
        if (!closeToZero(uu)) {
          val factor = uv / uu
          cforRange(0 until nC) { c =>
            m(j, c) := m(j, c) - factor * m(i, c)
          }
        }
      }
    }
  }

  protected def normalize(m: UMA)(implicit nroot: NRoot[A]): Unit = {
    cforRange(0 until m.rows) { r =>
      var norm2: A = scalar.zero
      cforRange(0 until m.cols) { c =>
        norm2 = norm2 + m(r, c) * m(r, c)
      }
      val normInv = spire.math.sqrt(norm2).reciprocal
      cforRange(0 until m.cols) { c =>
        m(r, c) := m(r, c) * normInv
      }
    }    
  }

  def orthonormalized(lhs: Mat[A])(implicit nroot: NRoot[A]): MA = {
    val res = UMA.fromMat(lhs)
    orthogonalize(res)
    normalize(res)
    result(res)
  }

  def inplaceRankFactorization(m: UMA): RankFactorization[A] { type MA = UMA } = {
    val used = collection.mutable.ArrayBuilder.make[Int]
    var r = 0
    cforRange(0 until m.cols) { c =>
      if (r < m.rows) {
        var priority = pivotA.priority(m(r, c))
        var pivot = r
        cforRange((r + 1) until m.rows) { r1 =>
          val r1Priority = pivotA.priority(m(r1, c))
          if (r1Priority > priority) {
            priority = r1Priority
            pivot = r1
          }
        }
        if (priority != 0) { // if el is zero, skip the column c
          used += c // keep track of bound variables

          // swap current row and pivot row
          cforRange(c until m.cols) { c1 =>
            val tmp = m(pivot, c1)
            m(pivot, c1) := m(r, c1)
            m(r, c1) := tmp
          }
          // normalize pivot row
          val f = m(r, c)
          cforRange(c until m.cols) { c1 =>
            m(r, c1) := m(r, c1) / f
          }
          // eliminate current column
          cforRange(0 until m.rows) { r1 =>
            if (r1 != r) {
              val g = m(r1, c)
              cforRange(c until m.cols) { c1 =>
                m(r1, c1) := m(r1, c1) - g * m(r, c1)
              }
            }
          }
          r += 1
        } else // set zero terms to exact zero (used for floating point)
          cforRange(r until m.rows) { r1 =>
            m(r, c) := scalar.zero
          }
      }
    }
    new RankFactorizationImpl(m, used.result)
  }

  final class RankFactorizationImpl(val rref: UMA, basis: Array[Int]) extends RankFactorization[A] {

    def rank = basis.length

    def basisIndex(k: Int) = basis(k)

    type MA = UMA

    def matC(original: Mat[A]): UMA = UMA.slice(original, ::, basis: Subscript)

    def matF: UMA = UMA.slice(rref, 0 until rank, ::)

  }

  def inplaceLU(lu: UMA): LUDecomposition[A] { type MA = UMA } = {
    val m = lu.rows // row dimension
    val n = lu.cols // column dimension
    require(m >= n)
    val piv = Array.tabulate(m)(identity) // internal storage of pivot vector
    var pCount = 0 // permutation count
                   // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

    // Outer loop.
    val luColj = alloc(m)

    cforRange(0 until n) { j =>
      // Make a copy of the j-th column to localize references.
      cforRange(0 until m) { i => luColj(i) := lu(i, j) }

      // Apply previous transformations.
      cforRange(0 until m) { i =>
        // Most of the time is spent in the following dot product.
        val kmax = min(i, j)
        var s = scalar.zero
        cforRange(0 until kmax) { k => // TODO: replace by vector operation when optimized
          s = s + lu(i, k) * luColj(k)
        }
        val nv = luColj(i) - s
        luColj(i) := nv
        lu(i, j) := nv
      }
      // Find pivot and exchange if necessary.
      var p = j
      var pPriority = pivotA.priority(luColj(j))
      cforRange(j + 1 until m) { i =>
        val iPriority = pivotA.priority(luColj(i))
        if (iPriority > pPriority) {
          p = i
          pPriority = iPriority
        }
      }
      if (p != j) {
        rowsPermute(lu, p, j)
        val t = piv(p)
        piv(p) = piv(j)
        piv(j) = t
        pCount += 1
      }

      // Compute multipliers.
      val diag = lu(j, j)
      if (j < m && pivotA.closeToZero(diag)) {
        cforRange(j + 1 until m) { i =>
          lu(i, j) := lu(i, j) / diag
        }
      }
    }
    new LUDecompositionImpl(piv, pCount, lu)
  }

  final class LUDecompositionImpl(
    val pivots: Array[Int],
    val permutationCount: Int,
    lu: UMA)(implicit pivotA: Pivot[A]) extends LUDecomposition[A] {

    def nPivots = pivots.length

    def pivot(k: Int) = pivots(k)

    type MA = UMA
    type VA = UVA

    val m = lu.rows // row dimension
    val n = lu.cols // column dimension

    lazy val pivotsInverse = {
      val res = new Array[Int](pivots.length)
      cforRange(0 until pivots.length) { i =>
        res(pivots(i)) = i
      }
      res
    }

    def permutation: UMA = {
      val p = UMA.zeros(n, n)
      cforRange(0 until n) { i => p(pivots(i), i) := scalar.one }
      p
    }

    def lower: UMA = {
      val l = UMA.zeros(m, n)
      cforRange(0 until n) { i => l(i, i) := scalar.one }
      cforRange(0 until m) { i =>
        cforRange(0 until i) { j =>
            l(i, j) := lu(i, j)
        }
      }
      l
    }

    def upper: UMA = {
      val u = UMA.zeros(n, n)
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
      var d = if ((permutationCount & 1) == 0) scalar.one else -scalar.one
      cforRange(0 until n) { j =>
        d *= lu(j, j)
      }
      d
    }

    def solve(b: Vec[A]): UVA = {
      require(lu.cols <= lu.rows)
      if (b.length != m)
        throw new IllegalArgumentException("Matrix row dimensions must agree.")
      if (isSingular)
        throw new RuntimeException("Matrix is singular.")
      // Copy right hand side with pivoting
      val x = UVA.slice(b, pivots)
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
      if (x.length > lu.cols) UVA.slice(x, 0 until lu.cols) else x
    }

    def solve(b: Mat[A]): UMA = {
      require(lu.cols <= lu.rows)
      if (b.rows != m)
        throw new IllegalArgumentException("Matrix row dimensions must agree.")
      if (isSingular)
        throw new RuntimeException("Matrix is singular.")
      val nx = b.cols
      // Copy right hand side with pivoting
      val x = UMA.slice(b, pivots, ::)

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
      if (x.rows > lu.cols) UMA.slice(x, 0 until lu.cols, ::) else x
    }

    def inverse: UMA = {
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
      colsPermuteInverse(r, pivotsInverse)
      r
    }
  }

}
