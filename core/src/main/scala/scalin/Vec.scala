package scalin

import spire.algebra._
import spire.syntax.cfor._

import scala.reflect.ClassTag

/** Vector trait; `A` is the scalar type. */
trait Vec[A] { lhs =>

  //// Abstract methods

  def length: Int

  def apply(k: Int): A

  //// Standard Java methods

  override def toString: String = Printer.vec(Vec.this)

  override def equals(any: Any): Boolean = any match {
    case rhs: Vec[_] => scalin.Vec.defaultEquals(lhs, rhs)
    case _ => false
  }

  override def hashCode: Int = scalin.Vec.defaultHashCode(lhs)

  //// Helper functions

  /** Returns a copy of this Vec if it has overlapping data with the object obj.
    *
    * Used in mutable operations where obj1 = op(op2) or op1 = op(op2, op3) and
    * op1 and op2/op3 share underlying data.
    * */
  def copyIfOverlap(obj: AnyRef): Vec[A]

  //// Conversion/creation

  def to[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): VA = ev.fromVec(lhs) // TODO: replace as in Mat

  def toRowMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.toRowMat(lhs)

  def toColMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.toColMat(lhs)

  def toDiagMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA], A: AdditiveMonoid[A]): MA = ev.toDiagMat(lhs)

  //// Collection-like methods

  def ++[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA]): VA = ev.cat(lhs, rhs)

  def cat[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA]): VA = ev.cat(lhs, rhs)

  def count(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.length) { k =>
      if (f(lhs(k)))
        n += 1
    }
    n
  }

  /** scala.collection-like flatMap. */
  def flatMap[B, VB <: Vec[B]](f: A => Vec[B])(implicit ev: VecEngine[B, VB]): VB = ev.flatMap[A](lhs)(f)

  /** Flatten the vector. */
  def flatten[AA](implicit U: Vec.Unpack.AuxA[A, AA], ev: VecEngine[AA, VAA] forSome { type VAA <: Vec[AA] }): ev.Ret = {
    import U.proof
    // type VAA has been lost, however, if we make VAA a type parameter of flatten, the implicit search fails,
    // probably because we look twice for an instance of Vec[_]
    // this hack recovers the correct return type
    // same hack used in Mat
    ev.flatten[U.V[U.A]](lhs).asInstanceOf[ev.Ret]
  }

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.length == 0) z else {
      var acc = op(z, lhs(0))
      cforRange(1 until lhs.length) { k =>
        acc = op(acc, lhs(k))
      }
      acc
    }

  /** Maps the values of the elements. */
  def map[B, VB <: Vec[B]](f: A => B)(implicit ev: VecEngine[B, VB]): VB = ev.map[A](lhs)(f)

  def toIndexedSeq: IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = lhs.length
    def apply(k: Int): A = lhs.apply(k)
  }

  /** Returns a new Array containing the elements of this Vec. */
  def toArray(implicit ev: ClassTag[A]): Array[A] = {
    val res = new Array[A](length)
    cforRange(0 until length) { i =>
      res(i) = apply(i)
    }
    res
  }

  //// Slices

  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecEngine[A, VA]): VA = ev.slice(lhs, sub)

  //// Shuffling elements around

  def reshape[MA <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatEngine[A, MA]): MA =
    ev.reshape(lhs, rows, cols)

  //// With `A:MultiplicativeMonoid`

  /** Product of all elements. */
  def product(implicit A: MultiplicativeMonoid[A]): A = fold(A.one)(A.times)

  /** Vector-scalar product. */
  def *[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.times(lhs, rhs)

  /** Scalar-vector product. */
  def *:[VA <: Vec[A]](realLhs: A)(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.times(realLhs, lhs)

  /** Kronecker product. */
  def kron[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.kron(lhs, rhs)

  /** Dyadic product by vector, which we don't call outer product, because we don't want to involve complex conjugation. */
  def dyad[MA <: Mat[A]](rhs: Vec[A])(implicit ev: MatEngine[A, MA], A: MultiplicativeMonoid[A]): MA = ev.dyad(lhs, rhs)

  //// With `A:AdditiveGroup`

  def +[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: AdditiveSemigroup[A]): VA = ev.plus(lhs, rhs)

  def -[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.minus(lhs, rhs)

  def unary_-[VA <: Vec[A]](implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.negate(lhs)

  /** Sum of all the elements in the vector. */
  def sum(implicit A: AdditiveMonoid[A]): A = fold(A.zero)(A.plus)

  /** Number of contained non-zero elements. */
  def nnz(implicit A: AdditiveMonoid[A], equ: Eq[A]): Int = count(A.isZero(_))

  //// With `A:Ring`

  /** Vector-matrix product, where vectors are interpreted as row vectors. */
  def *[VA <: Vec[A]](rhs: Mat[A])(implicit ev: VecEngine[A, VA], A: Semiring[A]): VA = ev.times(lhs, rhs)

  /** Dot product. Does not perform complex conjugation, thus it is equivalent to the real
    * inner product, but not the complex inner product.*/
  def dot(rhs: Vec[A])(implicit ev: VecEngine[A, _], A: Semiring[A]): A = ev.dot(lhs, rhs)

  //// With `A:GCDRing`

  /** Computes the gcd of the elements of the vector. */
  def gcd(implicit equ: Eq[A], A: GCDRing[A]): A = fold(A.zero)(A.gcd)

  /** Computes the lcm of the elements of the vector. */
  def lcm(implicit equ: Eq[A], A: GCDRing[A]): A = fold(A.one)(A.lcm)

  //// With `A:Field`

  def /[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: Field[A]): VA = ev.div(lhs, rhs)

  //// With `A:Conjugation`

  def conjugate[VA <: Vec[A]](implicit ev: VecEngine[A, VA], A: Involution[A]): VA = ev.conjugate(lhs)

  //// Pointwise operations

  //// Using standard Java methods

  /** Point-wise equality with scalar. */
  def pw_==[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  /** Point-wise equality with another vector of compatible size. */
  def pw_==[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  /** Point-wise nonequality with scalar. */
  def pw_!=[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  /** Point-wise nonequality with another vector of compatible size. */
  def pw_!=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  //// With `A:MultiplicativeMonoid`

  def pw_*[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.pointwiseTimes(lhs, rhs)

  //// With `A:Ring`

  def pw_+[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: AdditiveSemigroup[A]): VA = ev.pointwisePlus(lhs, rhs)

  def pw_-[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.pointwiseMinus(lhs, rhs)

  //// With `A:Field`

  def pw_/[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: Field[A]): VA = ev.pointwiseDiv(lhs, rhs)

  //// With `A:Eq`

  def pw_===[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def pw_===[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def pw_=!=[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)

  def pw_=!=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)
}

object Vec extends VecType[Vec] {

  trait Unpack[VA] {
    type V[X] <: Vec[X]
    type A
    implicit def proof: Vec[VA] =:= Vec[V[A]]
  }

  object Unpack {
    type AuxA[VA, A0] = Unpack[VA] { type A = A0 }
    def apply[VA](implicit U: Unpack[VA]): U.type {
      type V[X] = U.V[X]
      type A = U.A
    } = U
    implicit def unpack[V0[X] <: Vec[X], A0]: Unpack[V0[A0]] {
      type V[X] = V0[X]
      type A = A0
    } = new Unpack[V0[A0]] {
      type V[X] = V0[X]
      type A = A0
      def proof = implicitly
    }
  }

  def defaultEngine[A:TC] = scalin.immutable.DenseVec.defaultEngine[A]

  def defaultEquals(lhs: Vec[_], rhs: Vec[_]): Boolean = (lhs.length == rhs.length) && {
    cforRange(0 until lhs.length) { k =>
      if (lhs(k) != rhs(k)) return false
    }
    true
  }

  def defaultHashCode(lhs: Vec[_]): Int = {
    import scala.util.hashing.MurmurHash3._
    val seed = 0x3CA7195E
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until lhs.length) { k =>
      val hv = lhs(k).##
      if (hv != 0) {
        val hkv = k * 41 + hv
        a += hkv
        b *= (hkv | 1)
        n += 1
      }
    }
    var h = seed
    h = mix(h, lhs.length)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  def countTrue[A](lhs: Vec[A])(implicit ev: A =:= Boolean): Int = {
    import spire.syntax.cfor._
    var sum = 0
    cforRange(0 until lhs.length) { k => if (lhs(k): Boolean) sum += 1 }
    sum
  }

}
