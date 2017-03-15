package scalin

import spire.algebra._
import spire.syntax.cfor._

import algebra._

class PointwiseVec[A](val lhs: Vec[A]) extends AnyVal {

  //// Using standard Java methods

  def ==[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  //// With `A:MultiplicativeMonoid`

  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.pointwiseTimes(lhs, rhs)

  //// With `A:Ring`

  def +[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: AdditiveSemigroup[A]): VA = ev.pointwisePlus(lhs, rhs)

  def -[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.pointwiseMinus(lhs, rhs)

  //// With `A:Field`

  def /[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: Field[A]): VA = ev.pointwiseDiv(lhs, rhs)

  //// With `A:Eq`

  def ===[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecEngine[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)

}

final class ToVec[A, B](vec: Vec[A])(implicit conv: A => B) {

  def get[VB <: Vec[B]](implicit ev: VecEngine[B, VB]): VB = vec.map(conv)

}

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

  override def hashCode = scalin.Vec.defaultHashCode(lhs)

  //// Helper functions

  def pointwise: PointwiseVec[A] = new PointwiseVec[A](lhs)

  def copyIfOverlap(obj: AnyRef): Vec[A]

  //// Conversion/creation

  def to[B](implicit conv: A => B) = new ToVec[A, B](lhs)

  def toVec[VA <: Vec[A]](implicit ev: VecEngine[A, VA]): VA = ev.fromVec(lhs)

  def toRowMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.toRowMat(lhs)

  def toColMat[MA <: Mat[A]](implicit ev: MatEngine[A, MA]): MA = ev.toColMat(lhs)

  def toDiagMat[MA <: Mat[A]](implicit ev: MatRing[A, MA]): MA = ev.toDiagMat(lhs)

  //// Collection-like methods

  def ++[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA]): VA = ev.cat(lhs, rhs)

  def cat[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA]): VA = ev.cat(lhs, rhs)

  def count(f: A => Boolean)(implicit ev: VecEngine[A, _]): Int = ev.count(lhs)(f)

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

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ev: VecEngine[A, _]): A1 = ev.fold[A1](lhs)(z)(op)

  /** Maps the values of the elements. */
  def map[B, VB <: Vec[B]](f: A => B)(implicit ev: VecEngine[B, VB]): VB = ev.map[A](lhs)(f)

  def toIndexedSeq: IndexedSeq[A] = new IndexedSeq[A] {

    def length = lhs.length

    def apply(k: Int) = lhs.apply(k)

  }

  //// Slices

  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecEngine[A, VA]): VA = ev.slice(lhs, sub)

  //// Shuffling elements around

  def reshape[MA <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatEngine[A, MA]): MA =
    ev.reshape(lhs, rows, cols)

  //// With `A:MultiplicativeMonoid`

  /** Product of all elements. */
  def product(implicit ev: VecEngine[A, _], A: MultiplicativeMonoid[A]): A = ev.product(lhs)

  /** Vector-scalar product. */
  def *[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.times(lhs, rhs)

  /** Scalar-vector product. */
  def *:[VA <: Vec[A]](realLhs: A)(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.times(realLhs, lhs)

  /** Kronecker product. */
  def kron[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: MultiplicativeMonoid[A]): VA = ev.kron(lhs, rhs)

  /** Dyadic product by vector, which we don't call outer product, because we don't want to involve complex conjugation. */
  def dyad[MA <: Mat[A]](rhs: Vec[A])(implicit ev: MatMultiplicativeMonoid[A, MA]): MA = ev.dyad(lhs, rhs)

  //// With `A:AdditiveGroup`

  def +[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: AdditiveSemigroup[A]): VA = ev.plus(lhs, rhs)

  def -[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.minus(lhs, rhs)

  def unary_-[VA <: Vec[A]](implicit ev: VecEngine[A, VA], A: AdditiveGroup[A]): VA = ev.negate(lhs)

  def sum(implicit ev: VecEngine[A, _], A: AdditiveMonoid[A]): A = ev.sum(lhs)

  def nnz(implicit ev: VecEngine[A, _], eq: Eq[A], A: AdditiveMonoid[A]): Int = ev.nnz(lhs)

  //// With `A:Ring`

  /** Vector-matrix product, where vectors are interpreted as row vectors. */
  def *[VA <: Vec[A]](rhs: Mat[A])(implicit ev: VecEngine[A, VA], A: Semiring[A]): VA = ev.times(lhs, rhs)

  /** Dot product. Does not perform complex conjugation, thus it is equivalent to the real
    * inner product, but not the complex inner product.*/
  def dot(rhs: Vec[A])(implicit ev: VecEngine[A, _], A: Semiring[A]): A = ev.dot(lhs, rhs)

  //// With `A:EuclideanRing`

  def gcd(implicit ev: VecEngine[A, _], A: GCDRing[A], equ: Eq[A]): A = ev.gcd(lhs)

  def lcm(implicit ev: VecEngine[A, _], A: GCDRing[A], equ: Eq[A]): A = ev.lcm(lhs)

  //// With `A:Field`

  def /[VA <: Vec[A]](rhs: A)(implicit ev: VecEngine[A, VA], A: Field[A]): VA = ev.div(lhs, rhs)

}

object Vec {

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
