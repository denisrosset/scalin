package scalin
package algebra

import spire.algebra._

trait VecFactory[A, VA <: Vec[A]] {

  type Ret = VA // hack for the return type of Vec.flatten

  //// Creation

  def empty: VA

  def tabulate(length: Int)(f: Int => A): VA

  def fill(length: Int)(a: => A): VA

  def fillConstant(length: Int)(a: A): VA

  def fromSeq(elements: Seq[A]): VA

  def fromVec(vec: Vec[A]): VA

  //// Standard Java methods

  def equal(lhs: Vec[A], rhs: Vec[A]): Boolean

  /** Hashcode compatible with the reference algorithm provided in scalin.impl.VecFactory. */
  def hashCode(lhs: Vec[A]): Int

  //// Collection-like methods

  def count(lhs: Vec[A])(f: A => Boolean): Int

  def fold[A1 >: A](lhs: Vec[A])(z: A1)(op: (A1, A1) => A1): A1

  def map[B](lhs: Vec[B])(f: B => A): VA

  def cat(lhs: Vec[A], rhs: Vec[A]): VA

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA

  def flatten[B <: Vec[A]](lhs: Vec[B]): VA

  //// Slices

  def slice(vec: Vec[A], sub: Subscript): VA

  def slice(mat: Mat[A], sub: Subscript): VA

  /** Slices a vector from a matrix, for the row `r` and column subscript `cs`. */
  def rowSlice(mat: Mat[A], r: Int, cs: Subscript): VA

  /** Slices a vector from a matrix, for the column `c` and the row subscript `rs`. */
  def colSlice(mat: Mat[A], rs: Subscript, c: Int): VA

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA

  def pointwiseEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA

  def pointwiseEqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA

  def pointwiseEqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA

  def pointwiseNeqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA

  def pointwiseNeqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA

  //// With `Eq[A]`

  def eqv(lhs: Vec[A], rhs: Vec[A])(implicit eqv: Eq[A]): Boolean

}
