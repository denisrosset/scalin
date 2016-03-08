package scalin

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.ring._

import algebra._

class PointwiseVec[A](val lhs: Vec[A]) extends AnyVal {

  def ==[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[VB <: Vec[Boolean]](rhs: A)(implicit ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseNotEqual(lhs, rhs)

  def ===[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[VB <: Vec[Boolean]](rhs: A)(implicit A: Eq[A], ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[VB <: Vec[Boolean]](rhs: Vec[A])(implicit A: Eq[A], ev: VecTrait[Boolean, VB]): VB =
    ev.pointwiseNeqv(lhs, rhs)

  def +[VA <: Vec[A]](rhs: A)(implicit ev: VecRing[A, VA]): VA = ev.pointwisePlus(lhs, rhs)

  def -[VA <: Vec[A]](rhs: A)(implicit ev: VecRing[A, VA]): VA = ev.pointwiseMinus(lhs, rhs)

  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.pointwiseTimes(lhs, rhs)

  def /[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecField[A, VA]): VA = ev.pointwiseDiv(lhs, rhs)

}

/** Vector trait; `A` is the scalar type. */
trait Vec[A] { lhs =>

  override def toString: String = Printer.vec(Vec.this)

  override def equals(any: Any): Boolean = any match {
    case rhs: Vec[_] => (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (lhs(k) != rhs(k)) return false
      }
      true
    }
    case _ => false
  }

  def pointwise: PointwiseVec[A] = new PointwiseVec[A](lhs)

  def countTrue(implicit ev: A =:= Boolean): Int = {
    import spire.syntax.cfor._
    var sum = 0
    cforRange(0 until length) { k => if (apply(k): Boolean) sum += 1 }
    sum
  }

  def copyIfOverlap(obj: AnyRef): Vec[A]

  def length: Int

  def apply(k: Int): A

  // slices

  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecTrait[A, VA]): VA =
    ev.slice(lhs, sub)

  // shuffle

  def reshape[MA <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatTrait[A, MA]): MA =
    ev.reshape(lhs, rows, cols)

  // additive group

  def +[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.plus(lhs, rhs)

  def -[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.minus(lhs, rhs)

  def unary_-[VA <: Vec[A]](implicit ev: VecRing[A, VA]): VA = ev.negate(lhs)

  // multiplication by scalar

  def *[VA <: Vec[A]](rhs: A)(implicit ev: VecRing[A, VA]): VA = ev.times(lhs, rhs)

  def *:[VA <: Vec[A]](realLhs: A)(implicit ev: VecRing[A, VA]): VA = ev.times(realLhs, lhs)

  // vector-matrix product

  def *[VA <: Vec[A]](rhs: Mat[A])(implicit ev: VecRing[A, VA]): VA = ev.times(lhs, rhs)

  // kronecker product

  def kron[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecMultiplicativeMonoid[A, VA]): VA = ev.kron(lhs, rhs)

  // multiplication by vector (dot product)

  def dot[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): A = ev.dot(lhs, rhs)

  // multiplication by vector (dyadic product), which we don't call outer product, because we don't care about i.e. complex conjugation

  def dyad[MA <: Mat[A]](rhs: Vec[A])(implicit ev: MatMultiplicativeMonoid[A, MA]): MA = ev.dyad(lhs, rhs)

  def /[VA <: Vec[A]](rhs: A)(implicit ev: VecField[A, VA]): VA = ev.div(lhs, rhs)

  /** Flatten the vector. */
  def flatten[AA](implicit U: Vec.Unpack.AuxA[A, AA], ev: VecTrait[AA, VAA] forSome { type VAA <: Vec[AA] }): ev.Ret = {
    import U.proof
    // type VAA has been lost, however, if we make VAA a type parameter of flatten, the implicit search fails,
    // probably because we look twice for an instance of Vec[_]
    // this hack recovers the correct return type
    // same hack used in Mat
    ev.flatten[U.V[U.A]](lhs).asInstanceOf[ev.Ret]
  }

  /** Maps the values of the elements. */
  def map[B, VB <: Vec[B]](f: A => B)(implicit ev: VecTrait[B, VB]): VB =
    ev.map[A](lhs)(f)

  /** scala.collection-like flatMap. */
  def flatMap[B, VB <: Vec[B]](f: A => Vec[B])(implicit ev: VecTrait[B, VB]): VB = 
    ev.flatMap[A](lhs)(f)

  def count(f: A => Boolean)(implicit ev: VecTrait[A, _]): Int = ev.count(lhs)(f)

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1)(implicit ev: VecTrait[A, _]): A1 = ev.fold[A1](lhs)(z)(op)

  def toRowMat[MA <: Mat[A]](implicit ev: MatTrait[A, MA]): MA = ev.toRowMat(lhs)

  def toColMat[MA <: Mat[A]](implicit ev: MatTrait[A, MA]): MA = ev.toColMat(lhs)

  def nnz(implicit ev: VecRing[A, _], eq: Eq[A]): Int = ev.nnz(lhs)

  def sum(implicit ev: VecRing[A, _]): A = ev.sum(lhs)

  def product(implicit ev: VecMultiplicativeMonoid[A, _]): A = ev.product(lhs)

  def gcd(implicit ev: VecEuclideanRing[A, _]): A = ev.gcd(lhs)

  def lcm(implicit ev: VecEuclideanRing[A, _]): A = ev.lcm(lhs)

}

object Vec {

  trait Unpack[VA] {
    type V[A] <: Vec[A]
    type A
    implicit def proof: Vec[VA] =:= Vec[V[A]]
  }

  object Unpack {
    type AuxA[VA, A0] = Unpack[VA] { type A = A0 }
    def apply[VA](implicit U: Unpack[VA]): U.type {
      type V[A] = U.V[A]
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

}

