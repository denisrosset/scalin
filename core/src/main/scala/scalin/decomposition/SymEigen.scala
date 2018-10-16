package scalin.decomposition

import spire.algebra.{Field, NRoot, Signed}
import spire.syntax.cfor._
import spire.syntax.field._
import spire.syntax.signed._
import scalin.syntax.all._

class SymEigen[A:Epsilon:Field:Signed:NRoot](A: scalin.immutable.Mat[A]) extends Eigen[A] {
  require(A.nRows == A.nCols)

  import scalin.mutable.dense._

  val n = A.nRows
  val eps = Epsilon[A].eps

  private[this] val _V: scalin.mutable.Mat[A] = A.to[scalin.mutable.Mat[A]]
  private[this] val d: scalin.mutable.Vec[A] = scalin.mutable.Vec.zeros[A](n)
  private[this] val e: scalin.mutable.Vec[A] = scalin.mutable.Vec.zeros[A](n)

  tred2()
  tql2()

  // Symmetric Householder reduction to tridiagonal form.
  private[this] def tred2(): Unit = {
    //  This is derived from the Algol procedures tred2 by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    cforRange(0 until n) { j =>
      d(j) := _V(n-1, j)
    }

    // Householder reduction to tridiagonal form.
    cforRange(n - 1 until 0 by -1) { i =>

      // Scale to avoid under/overflow.

      var scale = Field[A].zero
      var h = Field[A].zero
      cforRange(0 until i) { k =>
        scale += d(k).abs
      }

      if (scale.isZero) {
        e(i) := d(i - 1)
        cforRange(0 until i) { j =>
          d(j) := _V(i - 1, j)
          _V(i, j) := Field[A].zero
          _V(j, i) := Field[A].zero
        }
      } else {
        // Generate Householder vector.
        cforRange(0 until i) { k =>
          d(k) := d(k) / scale
          h += d(k) * d(k)
        }

        var f = d(i - 1)
        var g = spire.math.sqrt(h)
        if (f > 0)
          g = -g
        e(i) := scale * g
        h = h - f * g
        d(i - 1) := f - g
        cforRange(0 until i) { j =>
          e(j) := Field[A].zero
        }

        // Apply similarity transformation to remaining columns.
        cforRange(0 until i) { j =>
          f = d(j)
          _V(j, i) := f
          g = e(j) + _V(j, j) * f
          cforRange(j + 1 to i - 1) { k =>
            g += _V(k, j) * d(k)
            e(k) := e(k) + _V(k, j) * f
          }
          e(j) := g
        }
        f = Field[A].zero
        cforRange(0 until i) { j =>
          e(j) := e(j) / h
          f += e(j) * d(j)
        }
        val hh = f / (h + h)
        cforRange(0 until i) { j =>
          e(j) := e(j) - hh * d(j)
        }
        cforRange(0 until i) { j =>
          f = d(j)
          g = e(j)
          cforRange(j to i - 1) { k =>
            _V(k, j) := _V(k, j) - (f * e(k) + g * d(k))
          }
          d(j) := _V(i-1, j)
          _V(i, j) := Field[A].zero
        }
      }
      d(i) := h
    }

    // Accumulate transformations.
    cforRange(0 until n - 1) { i =>
      _V(n-1, i) := _V(i, i)
      _V(i, i) := Field[A].one
      val h = d(i + 1)
      if (!h.isZero) {
        cforRange(0 to i) { k =>
          d(k) := _V(k, i+1) / h
        }
        cforRange(0 to i) { j =>
          var g = Field[A].zero
          cforRange(0 to i) { k =>
            g = g + _V(k, i+1) * _V(k, j)
          }
          cforRange(0 to i) { k =>
            _V(k, j) := _V(k, j) - g * d(k)
          }
        }
      }
      cforRange(0 to i) { k =>
        _V(k, i+1) := Field[A].zero
      }
    }
    cforRange(0 until n) { j =>
      d(j) := _V(n-1, j)
      _V(n-1, j) := Field[A].zero
    }
    _V(n-1, n-1) := Field[A].one
    e(0) := Field[A].zero
  }

  // Symmetric tridiagonal QL algorithm.
  private[this] def tql2(): Unit = {
    //  This is derived from the Algol procedures tql2, by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    cforRange(1 until n) { i =>
      e(i - 1) := e(i)
    }

    e(n - 1) := Field[A].zero

    var f = Field[A].zero
    var tst1 = Field[A].zero

    cforRange(0 until n) { l =>
      // Find small subdiagonal element
      tst1 = spire.math.max(tst1, d(l).abs + e(l).abs)
      var m = l
      var cont = true
      while (cont && m < n) {
        if (e(m).abs <= eps * tst1)
          cont = false
        else
          m += 1
      }

      // If m == l, d[l] is an eigenvalue,
      // otherwise, iterate.
      if (m > l) {
        var iter = 0
        do {
          iter += 1 // (Could check iteration count here.)

          // Compute implicit shift
          var g = d(l)
          var p = (d(l + 1) - g) / (e(l) + e(l)) // 2*e(l)
          var r = spire.math.hypot(p, Field[A].one)
          if (p < 0) r = -r
          d(l) := e(l) / (p + r)
          d(l + 1) := e(l) * (p + r)
          val dl1 = d(l + 1)
          var h = g - d(l)
          cforRange(l + 2 until n) { i =>
            d(i) := d(i) - h
          }
          f += h
          // Implicit QL transformation.
          p = d(m)
          var c = Field[A].one
          var c2 = c
          var c3 = c
          val el1 = e(l + 1)
          var s = Field[A].zero
          var s2 = Field[A].zero
          cforRange(m - 1 to l by - 1) { i =>
            c3 = c2
            c2 = c
            s2 = s
            g = c * e(i)
            h = c * p
            r = spire.math.hypot(p, e(i))
            e(i + 1) := s * r
            s = e(i) / r
            c = p / r
            p = c * d(i) - s * g
            d(i + 1) := h + s * (c * g + s * d(i))

            // Accumulate transformation.

            cforRange(0 until n) { k =>
              h = _V(k, i + 1)
              _V(k, i + 1) := s * _V(k, i) + c * h
              _V(k, i) := c * _V(k, i) - s * h
            }
          }
          p = -s * s2 * c3 * el1 * e(l) / dl1
          e(l) := s * p
          d(l) := c * p
          // Check for convergence.
        } while (e(l).abs > eps * tst1)
      }
      d(l) := d(l) + f
      e(l) := Field[A].zero
    }

    cforRange(0 until n -1) { i =>
      var k = i
      var p = d(i)
      cforRange(i + 1 until n) { j =>
        if (d(j) < p) {
          k = j
          p = d(j)
        }
      }
      if (k != i) {
        d(k) := d(i)
        d(i) := p
        cforRange(0 until n) { j =>
          p = _V(j, i)
          _V(j, i) := _V(j, k)
          _V(j, k) := p
        }
      }
    }

  }

  /** Return the real parts of the eigenvalues */
  def evRealPart: scalin.immutable.Vec[A] = {
    import scalin.immutable.dense._
    d.to[scalin.immutable.Vec[A]]
  }

  /** Return the imaginary parts of the eigenvalues */
  def evImagPart: scalin.immutable.Vec[A] = {
    import scalin.immutable.dense._
    e.to[scalin.immutable.Vec[A]]
  }

  /** Return the block diagonal eigenvalue matrix */
  lazy val D: scalin.immutable.Mat[A] = {
    import scalin.immutable.dense._
    scalin.immutable.Mat.tabulate(n, n) { (r, c) =>
      if (r == c) d(r)
      else if (c == r + 1 && e(r).isSignPositive) e(r)
      else if (c == r - 1 && e(r).isSignNegative) e(r)
      else Field[A].zero
    }
  }

  lazy val V: scalin.immutable.Mat[A] = {
    import scalin.immutable.dense._
    _V.to[scalin.immutable.Mat[A]]
  }

}
