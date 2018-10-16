package scalin

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._

import scala.reflect.ClassTag

/** Matrix trait. */
trait Mat[A] { lhs =>

  //// Abstract methods

  def nRows: Int

  def nCols: Int

  def apply(r: Int, c: Int): A

  //// Standard Java methods

  override def equals(any: Any): Boolean = any match {
    case rhs: Mat[_] => scalin.Mat.defaultEquals(lhs, rhs)
    case _ => false
  }

  override def hashCode: Int = scalin.Mat.defaultHashCode(lhs)

  override def toString: String = Printer.mat(Mat.this)

  //// Helper functions

  /** Returns a copy of this Mat if it has overlapping data with the object obj.
    *
    * Used in mutable operations where obj1 = op(op2) or op1 = op(op2, op3) and
    * op1 and op2/op3 share underlying data.
    * */
  def copyIfOverlap(obj: AnyRef): Mat[A]

  //// Conversion/creation

  def to[MA <: Mat[A]](implicit ev: MatConv[A, lhs.type, MA]): MA = ev(lhs)

  //// Collection-like methods

  /** Returns the number of elements satisfying the predicate `f`. */
  def count(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        if (f(lhs(r, c)))
          n += 1
      }
    }
    n
  }

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

  /** Folds the elements of the matrix using the specified associative binary operator.
    * 
    * The order in which operations are performed on elements is unspecified and 
    * may be nondeterministic. 
    */
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.nRows == 0 || lhs.nCols == 0) z
    else if (lhs.nRows == 1 && lhs.nCols == 1) lhs(0, 0)
    else {
      var acc = z // could be optimized
      // in column-major order
      cforRange(0 until lhs.nCols) { c =>
        cforRange(0 until lhs.nRows) { r =>
          acc = op(acc, lhs(r, c))
        }
      }
      acc
    }

    /** Maps the values of the elements. */
  def map[B, MB <: Mat[B]](f: A => B)(implicit ev: MatEngine[B, MB]): MB =
    ev.map[A](lhs)(f)

  def rowSeq[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): IndexedSeq[VA] = ev.rowSeq(lhs)

  def colSeq[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): IndexedSeq[VA] = ev.colSeq(lhs)

  /** Returns a new Array containing the elements of this Vec. */
  def toArrayArray(implicit ev: ClassTag[A]): Array[Array[A]] = {
    val res = new Array[Array[A]](nRows)
    cforRange(0 until nRows) { r =>
      res(r) = new Array[A](nCols)
      cforRange(0 until nCols) { c =>
        res(r)(c) = apply(r, c)
      }
    }
    res
  }

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

  /** Horizontal concatenation. */
  def horzcat[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA]): MA = ev.horzcat(lhs, rhs)

  /** Vertical concatenation. */
  def vertcat[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA]): MA = ev.vertcat(lhs, rhs)

  //// Properties

  def isSquare: Boolean = nRows == nCols

  //// Properties, with `A:Eq`

  def isSymmetric(implicit A: Eq[A]): Boolean = isSquare && {
      cforRange(0 until nRows) { r =>
        cforRange(0 until r) { c =>
          if (lhs(r, c) =!= lhs(c, r))
            return false
        }
      }
      true
    }

  def isHermitian(implicit A: Eq[A], ev: Involution[A]): Boolean = isSquare && {
    cforRange(0 until nRows) { r =>
      cforRange(0 to r) { c =>
        if (lhs(r, c) =!= ev.adjoint(lhs(c, r)))
          return false
      }
    }
    true
  }

  //// Properties, with `A:Eq` and `A:AdditiveMonoid`

  def isDiagonal(implicit A: Eq[A], ev: AdditiveMonoid[A]): Boolean = isSquare && {
    cforRange(0 until nRows) { r =>
      cforRange(0 until nCols) { c =>
        if (r != c && !ev.isZero(lhs(r, c)))
          return false
      }
    }
    true
  }

  //// With `A:MultiplicativeMonoid`

  /** Product by scalar from the right. */
  def *[MA <: Mat[A]](rhs: A)(implicit ev: MatEngine[A, MA], A: MultiplicativeSemigroup[A]): MA = ev.times(lhs, rhs)

  /** Product by scalar from the left. */
  def *:[MA <: Mat[A]](realLhs: A)(implicit ev: MatEngine[A, MA], A: MultiplicativeSemigroup[A]): MA = ev.times(realLhs, lhs)

  /** Kronecker product. */
  def kron[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: MultiplicativeSemigroup[A]): MA = ev.kron(lhs, rhs)

  def product(implicit A: MultiplicativeMonoid[A]): A = fold(A.one)(A.times)

  //// With `A:AdditiveGroup`

  /** Addition. */
  def +[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: AdditiveSemigroup[A]): MA = ev.plus(lhs, rhs)

  /** Subtraction. */
  def -[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: AdditiveGroup[A]): MA = ev.minus(lhs, rhs)

  /** Matrix opposite. */
  def unary_-[MA <: Mat[A]](implicit ev: MatEngine[A, MA], A: AdditiveGroup[A]): MA = ev.negate(lhs)

  /** Returns the number of non-zero elements in the matrix. */
  def nnz(implicit ev: Eq[A], A: AdditiveMonoid[A]): Int = count(A.isZero(_))

  /** Computes the sum of all the matrix elements. */
  def sum(implicit A: AdditiveMonoid[A]): A = fold(A.zero)(A.plus)

  /** Trace of the matrix, equal to the sum of diagonal entries. Requires a square matrix. */
  def trace(implicit A: AdditiveMonoid[A]): A = {
    val n = lhs.nRows
    require(n == lhs.nCols)
    if (n == 0) A.zero else {
      var s: A = lhs(0, 0)
      cforRange(1 until n) { k =>
        s = A.plus(s, lhs(k, k))
      }
      s
    }
  }

  //// With `A:Ring`

  /** Matrix multiplication. */
  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: Ring[A]): MA = ev.times(lhs, rhs)

  /** Matrix-vector product. The vector is interpreted as a column vector. */
  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: Ring[A]): VA = ev.times(lhs, rhs)

  /** Frobenius product: `A.frobenius(B) = trace(A * B.t)`. */
  def frobenius(rhs: Mat[A])(implicit ev: MatEngine[A, _], A: Ring[A]): A = ev.frobenius(lhs, rhs)

  //// With `A:EuclideanRing`

  /** Computes the gcd of the elements of the matrix. */
  def gcd(implicit equ: Eq[A], A: EuclideanRing[A]): A = fold(A.zero)(A.gcd)

  /** Computes the lcm of the elements of the matrix. */
  def lcm(implicit equ: Eq[A], A: EuclideanRing[A]): A = fold(A.one)(A.lcm)

  //// Methods for `A:Field`

  /** Division by scalar. */
  def /[MA <: Mat[A]](rhs: A)(implicit ev: MatEngine[A, MA], A: Field[A]): MA = ev.div(lhs, rhs)

  //// With `A:Conjugation`

  /** Conjugate. */
  def conjugate[MA <: Mat[A]](implicit ev: MatEngine[A, MA], A: Involution[A]): MA = ev.conjugate(lhs)

  /** Conjugate transpose. */
  def ct[MA <: Mat[A]](implicit ev: MatEngine[A, MA], A: Involution[A]): MA = ev.ct(lhs)

  //// Pointwise operations

  //// Using standard Java methods

  def pw_==[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def pw_==[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def pw_!=[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def pw_!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  //// With `A:MultiplicativeMonoid`

  def pw_*[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: MultiplicativeSemigroup[A]): MA = ev.pointwiseTimes(lhs, rhs)

  //// With `A:Ring`

  def pw_+[MA <: Mat[A]](rhs: A)(implicit ev: MatEngine[A, MA], A: AdditiveSemigroup[A]): MA = ev.pointwisePlus(lhs, rhs)

  def pw_-[MA <: Mat[A]](rhs: A)(implicit ev: MatEngine[A, MA], A: AdditiveGroup[A]): MA = ev.pointwiseMinus(lhs, rhs)

  //// With `A:Field`

  def pw_/[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatEngine[A, MA], A: Field[A]): MA = ev.pointwiseDiv(lhs, rhs)

  //// With `A:Eq`

  def pw_===[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def pw_===[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def pw_=!=[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def pw_=!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatEngine[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  //// Standard high level algorithms

  def inverse[MA <: Mat[A]](implicit ev: MatEngine[A, MA], ev1: algorithms.Inverse[A, MA]): MA = {
    identity(ev)
    ev1(lhs)
  }

}

object Mat extends MatType[Mat] {

  type TC[A] = Dummy[A]

  def defaultEngine[A:TC] = new immutable.DenseMat.Engine[A]

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

  def defaultEquals(lhs: Mat[_], rhs: Mat[_]): Boolean =
    (lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols) && {
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          if (lhs(r, c) != rhs(r, c)) return false
        }
      }
      true
    }

  def defaultHashCode(lhs: Mat[_]): Int = {
    import scala.util.hashing.MurmurHash3._
    val seed = 0x3CA7198A
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        val hv = lhs(r, c).##
        if (hv != 0) {
          val hkv = (r * 41 + c) * 41 + hv
          a += hkv
          b *= (hkv | 1)
          n += 1
        }
      }
    }
    var h = seed
    h = mix(h, lhs.nRows)
    h = mix(h, lhs.nCols)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  def countTrue[A](lhs: Mat[A])(implicit ev: A =:= Boolean): Int = {
    var sum = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        if (lhs(r, c): Boolean)
          sum += 1
      }
    }
    sum
  }

}
