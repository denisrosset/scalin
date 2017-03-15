package scalin
package algebra

import spire.algebra._

import spire.syntax.cfor._
import spire.syntax.multiplicativeMonoid._

import scalin.syntax.assign._

trait MatMultiplicativeMonoid[A, +MA <: Mat[A]] extends MatEngine[A, MA] {

  implicit def scalar: MultiplicativeMonoid[A]

  //// Creation

  def ones(rows: Int, cols: Int): MA = fillConstant(rows, cols)(scalar.one)

  //// With `MultiplicativeMonoid[A]`, returning scalar

  def product(lhs: Mat[A]): A = fold(lhs)(scalar.one)(scalar.times)

  //// With `MultiplicativeMonoid[A]`, returning matrix

  /** Scalar-matrix product. */
  def times(lhs: A, rhs: Mat[A]): MA = pointwiseUnary(rhs)(lhs * _)

  /** Matrix-scalar product. */
  def times(lhs: Mat[A], rhs: A): MA = pointwiseUnary(lhs)(_ * rhs)

  /** Pointwise multiplication, i.e. Hadamard product, see https://en.wikipedia.org/wiki/Hadamard_product_%28matrices%29 . */
  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A]): MA = pointwiseBinary(lhs, rhs)(_ * _)

  /** Dyadic product, see https://en.wikipedia.org/wiki/Dyadics#Dyadic.2C_outer.2C_and_tensor_products . 
    * 
    * Equivalent to the outer product when the scalars are reals (no complex conjugation is performed on
    * the inputs).
    */
  def dyad(lhs: Vec[A], rhs: Vec[A]): MA = tabulate(lhs.length, rhs.length) { (r, c) => lhs(r) * rhs(c) }

  /* Alternative
  def kron(lhs: Mat[A], rhs: Mat[A]): MA =
    tabulate(lhs.nRows * rhs.nRows, lhs.nCols * rhs.nCols) { (r, c) =>
      val rr = r % rhs.nRows
      val rl = r / rhs.nRows
      val cr = c % rhs.nCols
      val cl = c / rhs.nCols
      lhs(rl, cl) * rhs(rr, cr)
    }
   */
  /** Kronecker product. */
  def kron(x: Mat[A], y: Mat[A]): MA = {
    val nrx = x.nRows
    val ncx = x.nCols
    val nry = y.nRows
    val ncy = y.nCols
    val nR = nrx * nry
    val nC = ncx * ncy
    fromMutable(nR, nC) { b =>
      var r = 0
      cforRange(0 until nrx) { rx =>
        cforRange(0 until nry) { ry =>
          var c = 0
          cforRange(0 until ncx) { cx =>
            cforRange(0 until ncy) { cy =>
              b(r, c) := x(rx, cx) * y(ry, cy)
              c += 1
            }
          }
          r += 1
        }
      }
    }
  }

}
