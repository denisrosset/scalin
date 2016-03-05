package scalin
package algebra

import spire.algebra._
import spire.syntax.order._
import spire.syntax.cfor._

trait MatTrait[A, M[A] <: Mat[A]] {

  def scalar: Any

  type Extra[_]
  implicit def extra: Extra[A]
  def factory: MatFactory[M, Extra]

  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): M[A] = {
    val ri = rs.forLength(mat.rows)
    val ci = cs.forLength(mat.cols)
    factory.tabulate(ri.length, ci.length)( (k, l) => mat(ri(k), ci(l)) )
  }

  def pointwiseUnary(lhs: Mat[A])(f: A => A) = factory.tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c)) )

  def pointwiseBinary(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => A): M[A] = {
    require(lhs.rows == rhs.rows)
    require(lhs.cols == rhs.cols)
    factory.tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c), rhs(r, c)) )
  }

  def booleanBinaryAnd(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => Boolean): Boolean =
    (lhs.rows == rhs.rows && lhs.cols == rhs.cols) && {
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          if (!f(lhs(r, c), rhs(r, c))) return false
        }
      }
      true
    }

  def pointwiseBooleanUnary(lhs: Mat[A])(f: A => Boolean)(implicit extra: Extra[Boolean]): M[Boolean] =
    factory.tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c)) )

  def pointwiseBooleanBinary(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => Boolean)(implicit extra: Extra[Boolean]): M[Boolean] = {
    require(lhs.rows == rhs.rows && lhs.cols == rhs.cols)
    factory.tabulate(lhs.rows, lhs.cols)( (r, c) =>  f(lhs(r, c), rhs(r, c)) )
  }

  def equal(lhs: Mat[A], rhs: Mat[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ == _)

  def eqv(lhs: Mat[A], rhs: Mat[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

  def pointwiseEqual(lhs: Mat[A], rhs: A)(implicit extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual(lhs: Mat[A], rhs: Mat[A])(implicit extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual(lhs: Mat[A], rhs: A)(implicit extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual(lhs: Mat[A], rhs: Mat[A])(implicit extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv(lhs: Mat[A], rhs: A)(implicit A: Eq[A], extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv(lhs: Mat[A], rhs: Mat[A])(implicit A: Eq[A], extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv(lhs: Mat[A], rhs: A)(implicit A: Eq[A], extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv(lhs: Mat[A], rhs: Mat[A])(implicit A: Eq[A], extra: Extra[Boolean]): M[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

}

object MatTrait {

  type Aux[A, M[A] <: Mat[A], Extra0[_]] = MatTrait[A, M] { type Extra[A] = Extra0[A] }

}
