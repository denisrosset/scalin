package scalin

import spire.algebra._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  def ==[M[A] <: Mat[A], Extra[_]](rhs: A)(implicit ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def ==[M[A] <: Mat[A], Extra[_]](rhs: Mat[A])(implicit ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def !=[M[A] <: Mat[A], Extra[_]](rhs: A)(implicit ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def !=[M[A] <: Mat[A], Extra[_]](rhs: Mat[A])(implicit ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def ===[M[A] <: Mat[A], Extra[_]](rhs: A)(implicit A: Eq[A], ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def ===[M[A] <: Mat[A], Extra[_]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def =!=[M[A] <: Mat[A], Extra[_]](rhs: A)(implicit A: Eq[A], ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def =!=[M[A] <: Mat[A], Extra[_]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait.Aux[A, M, Extra], extra: Extra[Boolean]): M[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def +[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.pointwisePlus(lhs, rhs)

  def -[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.pointwiseMinus(lhs, rhs)

  def *[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.pointwiseTimes(lhs, rhs)

  def /[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M], field: Field[A]): M[A] = ev.pointwiseBinary(lhs, rhs)(field.div)

}

/** Concrete matrix trait. */
trait Mat[A] { lhs =>

  def copyIfOverlap(obj: AnyRef): Mat[A]

  def rows: Int

  def cols: Int

  def apply(r: Int, c: Int): A

//  def apply(r: Int, cs: Slice): Vec[A]

//  def apply(rs: Slice, c: Int): Vec[A]

//  def apply(rs: Slice, cs: Slice): Vec[A]

  override def toString: String = Printer.mat(Mat.this)

  def pointwise: PointwiseMat[A] = new PointwiseMat[A](lhs)

  def +[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.plus(lhs, rhs)

  def -[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.minus(lhs, rhs)

  def unary_-[M[A] <: Mat[A]](implicit ev: MatRing[A, M]): M[A] = ev.negate(lhs)

  def *[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.times(lhs, rhs)

  def *[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.times(lhs, rhs)

  def *:[M[A] <: Mat[A]](realLhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.times(realLhs, lhs)

  // we do not use a MatField type class, rather we multiply by the inverse, which is probably faster
  // TODO: make a proper type class
  def /[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M], field: Field[A]): M[A] = ev.times(lhs, field.reciprocal(rhs))

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    immutable.DenseMat.tabulate[A](rows, cols)(f)

}
