package scalin

import spire.algebra._
import spire.syntax.cfor._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  def ==[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatFactory[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def +[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwisePlus(lhs, rhs)

  def -[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwiseMinus(lhs, rhs)

  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.pointwiseTimes(lhs, rhs)

  def /[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatField[A, MA]): MA = ev.pointwiseDiv(lhs, rhs)

}

/** Matrix trait. */
trait Mat[A] { lhs =>

  //// Abstract methods

  def rows: Int

  def cols: Int

  def apply(r: Int, c: Int): A

  //// TODO: move somewhere

  def countTrue(implicit ev: A =:= Boolean): Int = {
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
  def apply[VA <: Vec[A]](r: Int, cs: Subscript)(implicit ev: VecFactory[A, VA]): VA =
    ev.rowSlice(lhs, r, cs)

  /** Column slice. */
  def apply[VA <: Vec[A]](rs: Subscript, c: Int)(implicit ev: VecFactory[A, VA]): VA =
    ev.colSlice(lhs, rs, c)

  /** Matrix-shaped slice. */
  def apply[MA <: Mat[A]](rs: Subscript, cs: Subscript)(implicit ev: MatFactory[A, MA]): MA =
    ev.slice(lhs, rs, cs)

  /** Slice flattening the matrix in column major order. */
  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecFactory[A, VA]): VA =
    ev.slice(lhs, sub)

  //// Methods without a special structure on the scalar

  /// ... returning `MA <: Mat[A]`

  /** Transposition. */
  def t[MA <: Mat[A]](implicit ev: MatFactory[A, MA]): MA = ev.t(lhs)

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
  def determinant(implicit ev: MatRing[A, _]): A = ev.determinant(lhs)

  //// Methods for `A:Field`

  /// ... returning `MA <: Mat[A]`

  /** Division by scalar. We do not use a MatField type class, rather we multiply by the inverse, 
    * which is probably faster anyway.
    */
  def /[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA], field: Field[A]): MA = ev.times(lhs, field.reciprocal(rhs))

  /** Flatten the matrix. */
  def flatten[AA](implicit U: Mat.Unpack.AuxA[A, AA], ev: MatFactory[AA, MAA] forSome { type MAA <: Mat[AA] }): ev.Ret = {
    import U.proof
    // type MAA has been lost, however, if we make MAA a type parameter of flatten, the implicit search fails,
    // probably because we look twice for an instance of Mat[_]
    // this hack recovers the correct return type
    // same hack used in Vec
    ev.flatten[U.M[U.A]](lhs).asInstanceOf[ev.Ret]
  }

  /** Maps the values of the elements. */
  def map[B, MB <: Mat[B]](f: A => B)(implicit ev: MatFactory[B, MB]): MB =
    ev.map[A](lhs)(f)

  /** scala.collection-like flatMap. */
  def flatMap[B, MB <: Mat[B]](f: A => Mat[B])(implicit ev: MatFactory[B, MB]): MB =
    ev.flatMap[A](lhs)(f)

  def count(f: A => Boolean)(implicit ev: MatFactory[A, _]): Int = ev.count(lhs)(f)

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ev: MatFactory[A, _]): A1 = ev.fold[A1](lhs)(z)(op)

  def nnz(implicit ev: MatRing[A, _], eq: Eq[A]): Int = ev.nnz(lhs)

  def sum(implicit ev: MatRing[A, _]): A = ev.sum(lhs)

  def product(implicit ev: MatMultiplicativeMonoid[A, _]): A = ev.product(lhs)

  def gcd(implicit ev: MatEuclideanRing[A, _]): A = ev.gcd(lhs)

  def lcm(implicit ev: MatEuclideanRing[A, _]): A = ev.lcm(lhs)

}

object Mat {

  trait Unpack[MA] {
    type M[X] <: Mat[X]
    type A
    implicit def proof: Mat[MA] =:= Mat[M[A]]
  }

  object Unpack {
    type AuxA[MA, A0] = Unpack[MA] { type A = A0 }
    def apply[MA](implicit U: Unpack[MA]): U.type {
      type M[X] = U.M[X]
      type A = U.A
    } = U
    implicit def unpack[M0[X] <: Mat[X], A0]: Unpack[M0[A0]] {
      type M[X] = M0[X]
      type A = A0
    } = new Unpack[M0[A0]] {
      type M[X] = M0[X]
      type A = A0
      def proof = implicitly
    }
  }

}
