package scalin
package algebra

import spire.algebra._
import spire.syntax.order._
import spire.syntax.cfor._

trait VecTrait[A, V[A] <: Vec[A]] {

  def scalar: Any

  type Extra[_]
  implicit def extra: Extra[A]
  def factory: VecFactory[V, Extra]

  def pointwiseUnary(lhs: Vec[A])(f: A => A) = factory.tabulate(lhs.length)(k => f(lhs(k)))

  def pointwiseBinary(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => A): V[A] = {
    require(lhs.length == rhs.length)
    factory.tabulate(lhs.length)( k =>  f(lhs(k), rhs(k)) )
  }

  def booleanBinaryAnd(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => Boolean): Boolean =
    (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (!f(lhs(k), rhs(k))) return false
      }
      true
    }

  def pointwiseBooleanUnary(lhs: Vec[A])(f: A => Boolean)(implicit extra: Extra[Boolean]): V[Boolean] =
    factory.tabulate(lhs.length)( k =>  f(lhs(k)) )

  def pointwiseBooleanBinary(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => Boolean)(implicit extra: Extra[Boolean]): V[Boolean] = {
    require(lhs.length == rhs.length)
    factory.tabulate(lhs.length)( k =>  f(lhs(k), rhs(k)) )
  }

  def equal(lhs: Vec[A], rhs: Vec[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ == _)

  def eqv(lhs: Vec[A], rhs: Vec[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

  def pointwiseEqual(lhs: Vec[A], rhs: A)(implicit extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual(lhs: Vec[A], rhs: Vec[A])(implicit extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual(lhs: Vec[A], rhs: A)(implicit extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual(lhs: Vec[A], rhs: Vec[A])(implicit extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv(lhs: Vec[A], rhs: A)(implicit A: Eq[A], extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv(lhs: Vec[A], rhs: Vec[A])(implicit A: Eq[A], extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv(lhs: Vec[A], rhs: A)(implicit A: Eq[A], extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv(lhs: Vec[A], rhs: Vec[A])(implicit A: Eq[A], extra: Extra[Boolean]): V[Boolean] =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  def hashCode(lhs: Vec[A]): Int = ???

}

object VecTrait {

  type Aux[A, V[A] <: Vec[A], Extra0[_]] = VecTrait[A, V] { type Extra[A] = Extra0[A] }

}
