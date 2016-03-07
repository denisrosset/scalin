package scalin

import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._

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

  def /[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA], field: Field[A]): VA = ev.pointwiseBinary(lhs, rhs)(field.div)

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

  // multiplication by vector (dot product)

  def dot[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): A = ev.dot(lhs, rhs)

  // multiplication by vector (dyadic product), which we don't call outer product, because we don't care about i.e. complex conjugation

  def dyad[MA <: Mat[A]](rhs: Vec[A])(implicit ev: MatMultiplicativeMonoid[A, MA]): MA = ev.dyad(lhs, rhs)

  // we do not use a VecField type class, rather we multiply by the inverse, which is probably faster
  // TODO: make a proper type class
  def /[VA <: Vec[A]](rhs: A)(implicit ev: VecRing[A, VA], field: Field[A]): VA = ev.times(lhs, field.reciprocal(rhs))

}

object Vec {

  def tabulate[A](length: Int)(f: Int => A): Vec[A] = immutable.DenseVec.tabulate[A](length)(f)

}
