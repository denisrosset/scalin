package scalin

import spire.algebra._
import spire.syntax.cfor._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  def ==[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def +[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwisePlus(lhs, rhs)

  def -[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwiseMinus(lhs, rhs)

  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.pointwiseTimes(lhs, rhs)

  def /[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA], field: Field[A]): MA = ev.pointwiseBinary(lhs, rhs)(field.div)

}

/** Matrix trait. */
trait Mat[A] { lhs =>

  //// Abstract methods

  def rows: Int

  def cols: Int

  def apply(r: Int, c: Int): A

  //// TODO: move somewhere

  def count(implicit ev: A =:= Boolean): Int = {
    import spire.syntax.cfor._
    var sum = 0
    cforRange(0 until rows) { r =>
      cforRange(0 until cols) { c =>
        if (apply(r, c): Boolean)
          sum += 1
      }
    }
    sum
  }

  //// Standard Java methods

  override def equals(any: Any): Boolean = any match {
    case rhs: Mat[_] => (lhs.rows == rhs.rows && lhs.cols == rhs.cols) && {
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          if (lhs(r, c) != rhs(r, c)) return false
        }
      }
      true
    }
    case _ => false
  }

  override def toString: String = Printer.mat(Mat.this)

  override def hashCode: Int = sys.error("Not implemented") // TODO

  //// Methods to deal with mutation

  def copyIfOverlap(obj: AnyRef): Mat[A]

  //// Syntax helpers

  def pointwise: PointwiseMat[A] = new PointwiseMat[A](lhs)

  //// Slices

  /** Row slice. */
  def apply[VA <: Vec[A]](r: Int, cs: Subscript)(implicit ev: VecTrait[A, VA]): VA =
    ev.rowSlice(lhs, r, cs)

  /** Column slice. */
  def apply[VA <: Vec[A]](rs: Subscript, c: Int)(implicit ev: VecTrait[A, VA]): VA =
    ev.colSlice(lhs, rs, c)

  /** Matrix-shaped slice. */
  def apply[MA <: Mat[A]](rs: Subscript, cs: Subscript)(implicit ev: MatTrait[A, MA]): MA =
    ev.slice(lhs, rs, cs)

  /** Slice flattening the matrix in column major order. */
  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecTrait[A, VA]): VA =
    ev.slice(lhs, sub)

  //// Methods without a special structure on the scalar

  /// ... returning `MA <: Mat[A]`

  /** Transposition. */
  def t[MA <: Mat[A]](implicit ev: MatTrait[A, MA]): MA = ev.t(lhs)

  //// Methods for `A:MultiplicativeMonoid`

  /** Kronecker product. */
  def kron[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatMultiplicativeMonoid[A, MA]): MA = ev.kron(lhs, rhs)

  //// Methods for `A:Ring`

  /// ... returning `MA <: Mat[A]`

  /** Addition. */
  def +[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.plus(lhs, rhs)

  /** Subtraction. */
  def -[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.minus(lhs, rhs)

  /** Matrix opposite. */
  def unary_-[MA <: Mat[A]](implicit ev: MatRing[A, MA]): MA = ev.negate(lhs)

  /** Matrix multiplication. */
  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  /** Product by scalar from the right. */
  def *[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  /** Product by scalar from the left. */
  def *:[MA <: Mat[A]](realLhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(realLhs, lhs)

  /// ... returning `VA <: Vec[A]`

  /** Matrix-vector product. The vector is interpreted as a column vector. */
  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.times(lhs, rhs)


  /// ... returning scalar `A`

  /** Frobenius product: `A.frobenius(B) = trace(A * B.t)`. */
  def frobenius(rhs: Mat[A])(implicit ev: MatRing[A, _]): A = ev.frobenius(lhs, rhs)

  /** Trace: sum of the diagonal elements. Requires a square matrix. */
  def trace(implicit ev: MatRing[A, _]): A = ev.trace(lhs)

  /** Determinant. Requires a square matrix. */
  def determinant(implicit ev: Determinant[A]): A = ev.determinant(lhs)

  //// Methods for `A:Field`

  /// ... returning `MA <: Mat[A]`

  /** Division by scalar. We do not use a MatField type class, rather we multiply by the inverse, 
    * which is probably faster anyway.
    */
  def /[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA], field: Field[A]): MA = ev.times(lhs, field.reciprocal(rhs))

}

object Mat {

  def tabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): Mat[A] =
    immutable.DenseMat.tabulate[A](rows, cols)(f)

}
