package scalin

import spire.algebra._

import algebra._

class PointwiseMat[A](val lhs: Mat[A]) extends AnyVal {

  def ==[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def ==[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: A)(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def !=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNotEqual(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def ===[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseEqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: A)(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def =!=[MB <: Mat[Boolean]](rhs: Mat[A])(implicit A: Eq[A], ev: MatTrait[Boolean, MB]): MB =
    ev.pointwiseNeqv(lhs, rhs)

  def +[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwisePlus(lhs, rhs)

  def -[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.pointwiseMinus(lhs, rhs)

  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.pointwiseTimes(lhs, rhs)

  def /[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA], field: Field[A]): MA = ev.pointwiseBinary(lhs, rhs)(field.div)

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
  def apply[VA <: Vec[A]](r: Int, cs: Subscript)(implicit ev: VecTrait[A, VA]): VA =
    ev.rowSlice(lhs, r, cs)

  def apply[VA <: Vec[A]](rs: Subscript, c: Int)(implicit ev: VecTrait[A, VA]): VA =
    ev.colSlice(lhs, rs, c)

  // nxn

  def apply[MA <: Mat[A]](rs: Subscript, cs: Subscript)(implicit ev: MatTrait[A, MA]): MA =
    ev.slice(lhs, rs, cs)

  // flattening

  def apply[VA <: Vec[A]](sub: Subscript)(implicit ev: VecTrait[A, VA]): VA =
    ev.slice(lhs, sub)

  // shuffle

  def t[MA <: Mat[A]](implicit ev: MatTrait[A, MA]): MA = ev.t(lhs)

  def pointwise: PointwiseMat[A] = new PointwiseMat[A](lhs)

  def +[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.plus(lhs, rhs)

  def -[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.minus(lhs, rhs)

  def unary_-[MA <: Mat[A]](implicit ev: MatRing[A, MA]): MA = ev.negate(lhs)

  def *[MA <: Mat[A]](rhs: Mat[A])(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  // matrix-vector product

  def *[VA <: Vec[A]](rhs: Vec[A])(implicit ev: VecRing[A, VA]): VA = ev.times(lhs, rhs)

  // product by scalar

  def *[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(lhs, rhs)

  def *:[MA <: Mat[A]](realLhs: A)(implicit ev: MatRing[A, MA]): MA = ev.times(realLhs, lhs)

  // we do not use a MatField type class, rather we multiply by the inverse, which is probably faster anyway
  def /[MA <: Mat[A]](rhs: A)(implicit ev: MatRing[A, MA], field: Field[A]): MA = ev.times(lhs, field.reciprocal(rhs))

}

object Mat {

  def tabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): Mat[A] =
    immutable.DenseMat.tabulate[A](rows, cols)(f)

}
