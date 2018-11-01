package scalin.decomposition

import scalin.util.ComplexDivision
import spire.algebra.{Field, NRoot, Signed}
import spire.syntax.cfor._
import spire.syntax.field._
import spire.syntax.signed._
import scalin.syntax.all._

/** Nonsymmetric real matrix eigenvalue decomposition, converted to Scala generic code from the Jama library
  * https://math.nist.gov/javanumerics/jama/
  */
class NonSymRealEigen[A:Epsilon:Field:Signed:NRoot](A: scalin.immutable.Mat[A]) extends RealEigen[A] {
  require(A.nRows == A.nCols)

  import scalin.mutable.dense._

  val n = A.nRows
  val eps = Epsilon[A].eps

  private[this] val H: scalin.mutable.Mat[A] = A.to[scalin.mutable.Mat[A]]
  private[this] val _V: scalin.mutable.Mat[A] = scalin.mutable.Mat.zeros[A](n, n)
  private[this] val d: scalin.mutable.Vec[A] = scalin.mutable.Vec.zeros[A](n)
  private[this] val e: scalin.mutable.Vec[A] = scalin.mutable.Vec.zeros[A](n)
  private[this] val ort: scalin.mutable.Vec[A] = scalin.mutable.Vec.zeros[A](n)

  orthes()
  hqr2()

  private[this] def orthes(): Unit = {
    //  This is derived from the Algol procedures orthes and ortran,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutines in EISPACK.
    val low = 0
    val high = n - 1

    cforRange(low + 1 to high - 1) { m =>
      // Scale column.
      var scale = Field[A].zero
      cforRange(m to high) { i =>
        scale = scale + H(i, m - 1).abs
      }
      if (!scale.isZero) {
        // Compute Householder transformation.
        var h = Field[A].zero
        cforRange(high to m by -1) { i =>
          ort(i) := H(i, m - 1) / scale
          h += ort(i) * ort(i)
        }
        var g = spire.math.sqrt(h)
        if (ort(m).isSignPositive) g = -g
        h = h - ort(m) * g
        ort(m) := ort(m) - g

        // Apply Householder similarity transformation
        // H = (I-u*u'/h)*H*(I-u*u')/h)

        cforRange(m until n) { j =>
          var f = Field[A].zero
          cforRange(high to m by -1) { i =>
            f += ort(i) * H(i, j)
          }
          f = f / h
          cforRange(m to high) { i =>
            H(i, j) := H(i, j) - f * ort(i)
          }
        }

        cforRange(0 to high) { i =>
          var f = Field[A].zero
          cforRange(high to m by -1) { j =>
            f += ort(j) * H(i, j)
          }
          f = f / h
          cforRange(m to high) { j =>
            H(i, j) := H(i, j) - f * ort(j)
          }
        }
        ort(m) := scale * ort(m)
        H(m, m - 1) := scale * g
      }
    }
    // Accumulate transformations (Algol's ortran).
    cforRange(0 until n) { i =>
      cforRange(0 until n) { j =>
        _V(i, j) := (if (i == j) Field[A].one else Field[A].zero)
      }
    }

    cforRange(high -1 to low + 1 by -1) { m =>
      if (!H(m, m-1).isZero) {
        cforRange(m + 1 to high) { i =>
          ort(i) := H(i, m-1)
        }
        cforRange(m to high) { j =>
          var g = Field[A].zero
          cforRange(m to high) { i =>
            g += ort(i) * _V(i, j)
          }
          // Double division avoids possible underflow
          g = (g / ort(m)) / H(m, m-1)
          cforRange(m to high) { i =>
            _V(i, j) := _V(i, j) + g * ort(i)
          }
        }
      }
    }
  }

  // Nonsymmetric reduction from Hessenberg to real Schur form.
  private[this] def hqr2(): Unit = {
    //  This is derived from the Algol procedure hqr2,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    // Initialize
    val cd = new ComplexDivision[A]
    val nn = this.n
    var n = nn - 1
    val low = 0
    val high = nn - 1
    var exshift = Field[A].zero
    var p = Field[A].zero
    var q = Field[A].zero
    var r = Field[A].zero
    var s = Field[A].zero
    var z = Field[A].zero
    var t = Field[A].zero
    var w = Field[A].zero
    var x = Field[A].zero
    var y = Field[A].zero

    // Store roots isolated by balanc and compute matrix norm

    var norm = Field[A].zero

    cforRange(0 until nn) { i =>
      if (i < low || i > high) {
        d(i) := H(i, i)
        e(i) := Field[A].zero
      }
      cforRange(spire.math.max(i - 1, 0) until nn) { j =>
        norm += H(i, j).abs
      }
    }

    // Outer loop over eigenvalue index
    var iter = 0

    while (n >= low) {

      // Look for single small sub-diagonal element

      var l = n
      var cont = true
      while (cont && l > low) {
        s = H(l - 1, l - 1).abs + H(l, l).abs
        if (s.isZero) s = norm
        if (H(l, l - 1).abs < eps * s)
          cont = false
        else
          l -= 1
      }

      // Check for convergence
      // One root found

      if (l == n) {
        H(n, n) := H(n, n) + exshift
        d(n) := H(n, n)
        e(n) := Field[A].zero
        n -= 1
        iter = 0

        // Two roots found

      } else if (l == n - 1) {
        w = H(n, n - 1) * H(n - 1, n)
        p = (H(n - 1, n - 1) - H(n, n)) / 2.0
        q = p * p + w
        z = spire.math.sqrt(q.abs)
        H(n, n) := H(n, n) + exshift
        H(n - 1, n - 1) := H(n - 1, n - 1) + exshift
        x = H(n, n)

        // Real pair

        if (q >= 0) {
          if (p >= 0)
            z = p + z
          else
            z = p - z
          d(n - 1) := x + z
          d(n) := d(n - 1)
          if (!z.isZero) d(n) := x - w / z
          e(n - 1) := Field[A].zero
          e(n) := Field[A].zero
          x = H(n, n - 1)
          s = x.abs + z.abs
          p = x / s
          q = z / s
          r = spire.math.sqrt(p*p + q*q)
          p = p / r
          q = q / r

          // Row modification

          cforRange(n - 1 until nn) { j =>
            z = H(n - 1, j)
            H(n - 1, j) := q * z + p * H(n, j)
            H(n, j) := q * H(n, j) - p * z
          }

          // Column modification

          cforRange(0 to n) { i =>
            z = H(i, n - 1)
            H(i, n - 1) := q * z + p * H(i, n)
            H(i, n) := q * H(i, n) - p * z
          }

          // Accumulate transformations

          cforRange(low to high) { i =>
            z = _V(i, n - 1)
            _V(i, n - 1) := q * z + p * _V(i, n)
            _V(i, n) := q * _V(i, n) - p * z
          }

          // Complex pair

        } else {
          d(n - 1) := x + p
          d(n) := x + p
          e(n - 1) := z
          e(n) := -z
        }
        n = n - 2
        iter = 0

        // No convergence yet

      } else {

        // Form shift

        x = H(n, n)
        y = Field[A].zero
        w = Field[A].zero
        if (l < n) {
          y = H(n - 1, n - 1)
          w = H(n, n - 1) * H(n - 1, n)
        }

        // Wilkinson's original ad hoc shift

        if (iter == 10) {
          exshift += x
          cforRange(low to n) { i =>
            H(i, i) := H(i, i) - x
          }
          s = H(n, n - 1).abs + H(n - 1, n - 2).abs
          x = s * Field.fromDouble[A](0.75)
          y = x
          w = Field.fromDouble[A](-0.4375) * s * s
        }

        // MATLAB's new ad hoc shift
        if (iter == 30) {
          s = (y - x) / Field.fromInt[A](2)
          s = s * s + w
          if (s > 0) {
            s = spire.math.sqrt(s)
            if (y < x) s = -s
            s = x - w / ((y - x) / Field.fromInt[A](2) + s)
            cforRange(low to n) { i =>
              H(i, i) := H(i, i) - s
            }
            exshift += s
            x = Field.fromDouble[A](0.964)
            y = x
            w = x
          }
        }

        iter = iter + 1 // (Could check iteration count here.)

        // Look for two consecutive small sub-diagonal elements
        var m = n - 2
        var cont = true
        while (cont && m >= l) {
          z = H(m, m)
          r = x - z
          s = y - z
          p = (r * s - w) / H(m + 1, m) + H(m, m + 1)
          q = H(m + 1, m + 1) - z - r - s
          r = H(m + 2, m + 1)
          s = p.abs + q.abs + r.abs
          p = p / s
          q = q / s
          r = r / s
          if (m == l)
            cont = false
          else if (H(m, m - 1).abs * (q.abs + r.abs) <
            eps * (p.abs * (H(m - 1, m - 1).abs + z.abs + H(m + 1, m + 1).abs)))
            cont = false
          else
            m -= 1
        }

        cforRange(m + 2 to n) { i =>
          H(i, i - 2) := Field[A].zero
          if (i > m + 2) H(i, i - 3) := Field[A].zero
        }

        // Double QR step involving rows l:n and columns m:n

        {
          var k = m
          var cont = true
          while (cont && k <= n - 1) {
            val notlast = (k != n - 1)
            if (k != m) {
              p = H(k, k - 1)
              q = H(k + 1, k - 1)
              r = if (notlast) H(k + 2, k - 1) else Field[A].zero
              x = p.abs + q.abs + r.abs
              if (!x.isZero) {
                p = p / x
                q = q / x
                r = r / x
              }
            }
            if (x.isZero)
              cont = false
            else {
              s = spire.math.sqrt(p * p + q * q + r * r)
              if (p < 0) s = -s
              if (s != 0) {
                if (k != m)
                  H(k, k - 1) := -s * x
                else if (l != m)
                  H(k, k - 1) := -H(k, k - 1)
                p = p + s
                x = p / s
                y = q / s
                z = r / s
                q = q / p
                r = r / p

                // Row modification

                cforRange(k until nn) { j =>
                  p = H(k, j) + q * H(k + 1, j)
                  if (notlast) {
                    p = p + r * H(k + 2, j)
                    H(k + 2, j) := H(k + 2, j) - p * z
                  }
                  H(k, j) := H(k, j) - p * x
                  H(k + 1, j) := H(k + 1, j) - p * y
                }

                // Column modification

                cforRange(0 to spire.math.min(n, k + 3)) { i =>
                  p = x * H(i, k) + y * H(i, k + 1)
                  if (notlast) {
                    p = p + z * H(i, k + 2)
                    H(i, k + 2) := H(i, k + 2) - p * r
                  }
                  H(i, k) := H(i, k) - p
                  H(i, k + 1) := H(i, k + 1) - p * q
                }

                // Accumulate transformations

                cforRange(low to high) { i =>
                  p = x * _V(i, k) + y * _V(i, k + 1)
                  if (notlast) {
                    p = p + z * _V(i, k + 2)
                    _V(i, k + 2) := _V(i, k + 2) - p * r
                  }
                  _V(i, k) := _V(i, k) - p
                  _V(i, k + 1) := _V(i, k + 1) - p * q
                }
              } // s != 0
              k += 1
            }
          } // k loop
        } // k loop closing
      } // check convergence
    } // while (n >= low)

    // Backsubstitute to find vectors of upper triangular form

    if (norm.isZero) return

    cforRange(nn - 1 to 0 by -1) { n =>
      p = d(n)
      q = e(n)

      // Real vector

      if (q.isZero) {
        var l = n
        H(n, n) := Field[A].one
        cforRange(n - 1 to 0 by -1) { i =>
          w = H(i, i) - p
          r = Field[A].zero
          cforRange(l to n) { j =>
            r = r + H(i, j) * H(j, n)
          }
          if (e(i).isSignNegative()) {
            z = w
            s = r
          } else {
            l = i
            if (e(i).isZero) {
              if (!w.isZero)
                H(i, n) := -r / w
              else
                H(i, n) := -r / (eps * norm)

              // Solve real equations

            } else {
              x = H(i, i + 1)
              y = H(i + 1, i)
              q = (d(i) - p) * (d(i) - p) + e(i) * e(i)
              t = (x * s - z * r) / q
              H(i, n) := t
              if (x.abs > z.abs)
                H(i + 1, n) := (-r - w * t) / x
              else
                H(i + 1, n) := (-s - y * t) / z
            }

            // Overflow control

            t = H(i, n).abs
            if ((eps * t) * t > Field[A].one) {
              cforRange(i to n) { j =>
                H(j, n) := H(j, n) / t
              }
            }
          }
        }

        // Complex vector
      } else if (q.isSignNegative) {
        var l = n - 1

        // Last vector component imaginary so matrix is triangular

        if (H(n, n - 1).abs > H(n - 1, n).abs) {
          H(n - 1, n - 1) := q / H(n, n - 1)
          H(n - 1, n) := -H(n, n) - p / H(n, n - 1)
        } else {
          cd.cdiv(Field[A].zero, -H(n - 1, n), H(n - 1, n - 1) - p, q)
          H(n - 1, n - 1) := cd.cdivr
          H(n - 1, n) := cd.cdivi
        }
        H(n, n - 1) := Field[A].zero
        H(n, n) := Field[A].one
        cforRange(n - 2 to 0 by -1) { i =>
          var ra = Field[A].zero
          var sa = Field[A].zero
          var vr = Field[A].zero
          var vi = Field[A].zero
          cforRange(l to n) { j =>
            ra = ra + H(i, j) * H(j, n - 1)
            sa = sa + H(i, j) * H(j, n)
          }
          w = H(i, i) - p
          if (e(i).isSignNegative) {
            z = w
            r = ra
            s = sa
          } else {
            l = i
            if (e(i).isZero) {
              cd.cdiv(-ra, -sa, w, q)
              H(i, n - 1) := cd.cdivr
              H(i, n) := cd.cdivi
            } else {

              // Solve complex equations

              x = H(i, i + 1)
              y = H(i + 1, i)
              vr = (d(i) - p) * (d(i) - p) + e(i) * e(i) - q * q
              vi = (d(i) - p) * Field.fromInt[A](2) * q
              if (vr.isZero & vi.isZero)
                vr = eps * norm * (w.abs + q.abs + x.abs + y.abs + z.abs)
              cd.cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
              H(i, n - 1) := cd.cdivr
              H(i, n) := cd.cdivi
              if (x.abs > z.abs + q.abs) {
                H(i + 1, n - 1) := (-ra - w * H(i, n - 1) + q * H(i, n)) / x
                H(i + 1, n) := (-sa - w * H(i, n) - q * H(i, n - 1)) / x
              } else {
                cd.cdiv(-r - y * H(i, n - 1), -s - y * H(i, n), z, q)
                H(i + 1, n - 1) := cd.cdivr
                H(i + 1, n) := cd.cdivi
              }
            }

            // Overflow control

            t = spire.math.max(H(i, n - 1).abs, H(i, n).abs)
            if ((eps * t) * t > 1) {
              cforRange(i to n) { j =>
                H(j, n - 1) := H(j, n - 1) / t
                H(j, n) := H(j, n) / t
              }
            }
          }
        }
      }
    }

    // Vectors of isolated roots

    cforRange(0 until nn) { i =>
      if (i < low | i > high) {
        cforRange(i until nn) { j =>
          _V(i, j) := H(i, j)
        }
      }
    }

    // Back transformation to get eigenvectors of original matrix

    cforRange(nn - 1 to low by -1) { j =>
      cforRange(low to high) { i =>
        z = Field[A].zero
        cforRange(low to spire.math.min(j, high)) { k =>
          z = z + _V(i, k) * H(k, j)
        }
        _V(i, j) := z
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
