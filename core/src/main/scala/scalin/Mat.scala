package scalin

import spire.algebra._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  //// Using standard Java methods

  def ==[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  //// With `A:MultiplicativeMonoid`

  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.pointwiseTimes(lhs, rhs)

  //// With `A:Ring`

  def +[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwisePlus(lhs, rhs)

  def -[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwiseMinus(lhs, rhs)

  //// With `A:Field`

  def /[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatField[A, MA]): MA = ev.pointwiseDiv(lhs, rhs)

  //// With `A:Eq`

  def ===[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

}

final class ToMat[A, B](val mat: Mat[A])(implicit conv: A => B) {

  def get[MB <: Mat[B]](implicit ev: MatEngine[B, MB]): MB = mat.map(conv)

}

/** Matrix trait. */
trait Mat[A] { lhs =>

  //// Abstract methods

  def nRows: Int

  def nCols: Int

  def apply(r: Int, c: Int): A

  //// Standard Java methods

  override def equals(any: Any): Boolean = any match {
    case rhs: Mat[_] => scalin.impl.Mat.equal(lhs, rhs)
    case _ => false
  }

  override def hashCode: Int = scalin.impl.Mat.hashCode(lhs)

  override def toString: String = Printer.mat(Mat.this)

  //// Helper functions

  def pointwise: PointwiseMat[A] = new PointwiseMat[A](lhs)

  def copyIfOverlap(obj: AnyRef): Mat[A]

  //// Conversion/creation

  def to[B](implicit conv: A => B) = new ToMat[A, B](lhs)

  def toMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.fromMat(lhs)

  //// Collection-like methods

  def count(f: A => Boolean)(implicit ev: MatEngine[A, _]): Int = ev.count(lhs)(f)

  /** scala.collection-like flatMap. */
  def flatMap[B, MB <: Mat[B]](f: A => Mat[B])(implicit ev: MatEngine[B, MB]): MB =
    ev.flatMap[A](lhs)(f)

  /** Flatten the matrix. */
  def flatten[AA](implicit U: Mat.Unpack.AuxA[A, AA], ev: MatEngine[AA, MAA] forSome { type MAA <: Mat[AA] }): ev.Ret = {
    import U.proof
    // type MAA has been lost, however, if we make MAA a type parameter of flatten, the implicit search fails,
    // probably because we look twice for an instance of Mat[_]
    // this hack recovers the correct return type
    // same hack used in Vec
    ev.flatten[U.M[U.A]](lhs).asInstanceOf[ev.Ret]
  }

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ev: MatEngine[A, _]): A1 = ev.fold[A1](lhs)(z)(op)

    /** Maps the values of the elements. */
  def map[B, MB <: Mat[B]](f: A => B)(implicit ev: MatEngine[B, MB]): MB =
    ev.map[A](lhs)(f)

  def rowsSeq[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): IndexedSeq[VA] = ev.rowSeq(lhs)

  def colSeq[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): IndexedSeq[VA] = ev.colSeq(lhs)

  //// Slices

  /** Row slice. */
  def apply[VA <: Vec[A]](r: Int, cs: Subscript)(implicit ev: VecEngine[A, VA]): VA =
    ev.rowSlice(lhs, r, cs)

  /** Column slice. */
  def apply[VA <: Vec[A]](rs: Subscript, c: Int)(implicit ev: VecEngine[A, VA]): VA =
    ev.colSlice(lhs, rs, c)

  /** Matrix-shaped slice. */
  def apply[MA <: Mat[A]](rs: Subscript, cs: Subscript)(implicit ev: MatEngine[A, MA]): MA =
    ev.slice(lhs, rs, cs)

  /** Slice flattening the matrix in column major order. */
  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecEngine[A, VA]): VA =
    ev.slice(lhs, sub)

  def diag[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): VA =
    ev.diag(lhs)

  //// Shuffling elements around

  /** Transposition. */
  def t[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.t(lhs)

  def horzcat[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA]): MA = ev.horzcat(lhs, rhs)

  def vertcat[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA]): MA = ev.vertcat(lhs, rhs)

  //// With `A:MultiplicativeMonoid`

  /** Product by scalar from the right. */
  def *[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  /** Product by scalar from the left. */
  def *:[MA <: Mat[A]](realLhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(realLhs, lhs)

  /** Kronecker product. */
  def kron[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatMultiplicativeMonoid[A, MA]): MA = ev.kron(lhs, rhs)

  def product(implicit ev: MatMultiplicativeMonoid[A, _]): A = ev.product(lhs)

  //// With `A:AdditiveGroup`

  /** Addition. */
  def +[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.plus(lhs, rhs)

  /** Subtraction. */
  def -[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.minus(lhs, rhs)

  /** Matrix opposite. */
  def unary_-[MA <: Mat[A]](implicit ev: MatRing[A, MA]): MA = ev.negate(lhs)

  /** Returns the number of non-zero elements in the matrix. */
  def nnz(implicit ev: MatRing[A, _], eq: Eq[A]): Int = ev.nnz(lhs)

  /** Computes the sum of all the matrix elements. */
  def sum(implicit ev: MatRing[A, _]): A = ev.sum(lhs)

  /** Trace: sum of the diagonal elements. Requires a square matrix. */
  def trace(implicit ev: MatRing[A, _]): A = ev.trace(lhs)

  //// With `A:Ring`

  /** Matrix multiplication. */
  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  /** Matrix-vector product. The vector is interpreted as a column vector. */
  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.times(lhs, rhs)

  /** Frobenius product: `A.frobenius(B) = trace(A * B.t)`. */
  def frobenius(rhs: Mat[A])(implicit ev: MatRing[A, _]): A = ev.frobenius(lhs, rhs)

  /** Computes the matrix determinant. Requires a square matrix. */
  def determinant(implicit ev: MatRing[A, _]): A = ev.determinant(lhs)

  //// With `A:EuclideanRing`

  def gcd(implicit ev: MatEuclideanRing[A, _]): A = ev.gcd(lhs)

  def lcm(implicit ev: MatEuclideanRing[A, _]): A = ev.lcm(lhs)

  def orthogonalized[MA <: Mat[A]](implicit ev: MatEuclideanRing[A, MA]): MA = ev.orthogonalized(lhs)

  def rank(implicit ev: MatEuclideanRing[A, _]): Int = ev.rank(lhs)

  //// Methods for `A:Field`

  /** Division by scalar. */
  def /[MA <: Mat[A]](rhs: A)(implicit ev: MatField[A, MA]): MA = ev.div(lhs, rhs)

  def luDecomposition(implicit ev: MatField[A, _]): LUDecomposition[A] = ev.luDecomposition(lhs)

  def rankFactorization(implicit ev: MatField[A, _]): RankFactorization[A] = ev.rankFactorization(lhs)

  def inverse[MA <: Mat[A]](implicit ev: MatField[A, MA]): MA = ev.inverse(lhs)
  
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
