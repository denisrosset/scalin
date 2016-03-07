package scalin

import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._

import algebra._

class PointwiseVec[A](val lhs: Vec[A]) extends AnyVal {

  def ==[V[A] <: Vec[A]](rhs: A)(implicit ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def ==[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def !=[V[A] <: Vec[A]](rhs: A)(implicit ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def !=[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def ===[V[A] <: Vec[A]](rhs: A)(implicit A: Eq[A], ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def ===[V[A] <: Vec[A]](rhs: Vec[A])(implicit A: Eq[A], ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def =!=[V[A] <: Vec[A]](rhs: A)(implicit A: Eq[A], ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def =!=[V[A] <: Vec[A]](rhs: Vec[A])(implicit A: Eq[A], ev: VecTrait[Boolean, V]): V[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def +[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.pointwisePlus(lhs, rhs)

  def -[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.pointwiseMinus(lhs, rhs)

  def *[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V]): V[A] = ev.pointwiseTimes(lhs, rhs)

  def /[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V], field: Field[A]): V[A] = ev.pointwiseBinary(lhs, rhs)(field.div)

}

/** Vector trait; `A` is the scalar type. */
trait Vec[A] { lhs =>

  override def toString: String = Printer.vec(Vec.this)

  def pointwise: PointwiseVec[A] = new PointwiseVec[A](lhs)

  def count(implicit ev: A =:= Boolean): Int = {
    import spire.syntax.cfor._
    var sum = 0
    cforRange(0 until length) { k => if (apply(k): Boolean) sum += 1 }
    sum
  }

  def product(implicit A: MultiplicativeMonoid[A]): A =
    if (length == 0) A.one else {
      var p = apply(0)
      cforRange(1 until length) { k => p *= apply(k) }
      p
    }

  def sum(implicit A: AdditiveMonoid[A]): A =
    if (length == 0) A.zero else {
      var s = apply(0)
      cforRange(1 until length) { k => s += apply(k) }
      s
    }

  def copyIfOverlap(obj: AnyRef): Vec[A]

  def length: Int

  def apply(k: Int): A

  // slices

  def apply[V[A] <: Vec[A]](sub: Subscript)(implicit ev: VecTrait[A, V]): V[A] =
    ev.slice(lhs, sub)

  // shuffle

  def reshape[M[A] <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatTrait[A, M]): M[A] =
    ev.reshape(lhs, rows, cols)

  // additive group

  def +[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V]): V[A] = ev.plus(lhs, rhs)

  def -[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V]): V[A] = ev.minus(lhs, rhs)

  def unary_-[V[A] <: Vec[A]](implicit ev: VecRing[A, V]): V[A] = ev.negate(lhs)

  // multiplication by scalar

  def *[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.times(lhs, rhs)

  def *:[V[A] <: Vec[A]](realLhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.times(realLhs, lhs)

  // vector-matrix product

  def *[V[A] <: Vec[A]](rhs: Mat[A])(implicit ev: VecRing[A, V]): V[A] = ev.times(lhs, rhs)

  // multiplication by vector (dot product)

  def dot[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V]): A = ev.dot(lhs, rhs)

  // multiplication by vector (dyadic product), which we don't call outer product, because we don't care about i.e. complex conjugation

  def dyad[M[A] <: Mat[A]](rhs: Vec[A])(implicit ev: MatMultiplicativeMonoid[A, M]): M[A] = ev.dyad(lhs, rhs)

  // we do not use a VecField type class, rather we multiply by the inverse, which is probably faster
  // TODO: make a proper type class
  def /[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V], field: Field[A]): V[A] = ev.times(lhs, field.reciprocal(rhs))

}

object Vec {

  def tabulate[A](length: Int)(f: Int => A): Vec[A] = immutable.DenseVec.tabulate[A](length)(f)

}
