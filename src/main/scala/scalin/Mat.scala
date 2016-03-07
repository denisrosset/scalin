package scalin

import spire.algebra._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  def ==[M[A] <: Mat[A]](rhs: A)(implicit ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def ==[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseEqual(lhs, rhs)

  def !=[M[A] <: Mat[A]](rhs: A)(implicit ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def !=[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseNotEqual(lhs, rhs)

  def ===[M[A] <: Mat[A]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def ===[M[A] <: Mat[A]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseEqv(lhs, rhs)

  def =!=[M[A] <: Mat[A]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def =!=[M[A] <: Mat[A]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, M]): M[Boolean] = ev.pointwiseNeqv(lhs, rhs)

  def +[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.pointwisePlus(lhs, rhs)

  def -[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.pointwiseMinus(lhs, rhs)

  def *[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.pointwiseTimes(lhs, rhs)

  def /[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M], field: Field[A]): M[A] = ev.pointwiseBinary(lhs, rhs)(field.div)

}

/** Matrix trait. */
trait Mat[A] { lhs =>

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

  override def toString: String = Printer.mat(Mat.this)

  def copyIfOverlap(obj: AnyRef): Mat[A]

  def rows: Int

  def cols: Int

  // 1x1

  def apply(r: Int, c: Int): A

  // 1xn and nx1
  def apply[V[A] <: Vec[A]](r: Int, cs: Subscript)(implicit ev: VecTrait[A, V]): V[A] =
    ev.rowSlice(lhs, r, cs)

  def apply[V[A] <: Vec[A]](rs: Subscript, c: Int)(implicit ev: VecTrait[A, V]): V[A] =
    ev.colSlice(lhs, rs, c)

  // nxn

  def apply[M[A] <: Mat[A]](rs: Subscript, cs: Subscript)(implicit ev: MatTrait[A, M]): M[A] =
    ev.slice(lhs, rs, cs)

  // flattening

  def apply[V[A] <: Vec[A]](sub: Subscript)(implicit ev: VecTrait[A, V]): V[A] =
    ev.slice(lhs, sub)

  // shuffle

  def t[M[A] <: Mat[A]](implicit ev: MatTrait[A, M]): M[A] = ev.t(lhs)

  def pointwise: PointwiseMat[A] = new PointwiseMat[A](lhs)

  def +[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.plus(lhs, rhs)

  def -[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.minus(lhs, rhs)

  def unary_-[M[A] <: Mat[A]](implicit ev: MatRing[A, M]): M[A] = ev.negate(lhs)

  def *[M[A] <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, M]): M[A] = ev.times(lhs, rhs)

  // matrix-vector product

  def *[V[A] <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, V]): V[A] = ev.times(lhs, rhs)

  // product by scalar

  def *[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.times(lhs, rhs)

  def *:[M[A] <: Mat[A]](realLhs: A)(implicit ev: MatRing[A, M]): M[A] = ev.times(realLhs, lhs)

  // we do not use a MatField type class, rather we multiply by the inverse, which is probably faster anyway
  def /[M[A] <: Mat[A]](rhs: A)(implicit ev: MatRing[A, M], field: Field[A]): M[A] = ev.times(lhs, field.reciprocal(rhs))

}

object Mat {

  def tabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): Mat[A] =
    immutable.DenseMat.tabulate[A](rows, cols)(f)

}
