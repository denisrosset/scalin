package scalin
package mutable

import spire.algebra._
import spire.syntax.field._
import spire.syntax.cfor._

import immutable.ConstantVec

trait AbstractVec[A] extends scalin.AbstractVec[A] { lhs =>

  def update(k: Int, a: A): Unit

  protected def set(rhs: scalin.AbstractVec[A]): Unit = {
    cforRange(0 until length) { k =>
      update(k, rhs(k))
    }
  }

  protected def copyIfNeeded(rhs: scalin.AbstractVec[A]): scalin.AbstractVec[A] = rhs.touch(AbstractVec.this) match {
    case Touch.Clean() | Touch.AsIs() => rhs
    case _ => rhs.get
  }

  def :=(rhs: scalin.AbstractVec[A]): Unit = set(copyIfNeeded(rhs))

  def :=(rhs: A): Unit = this := ConstantVec(rhs, length)

  def +=(rhs: scalin.AbstractVec[A])(implicit A: AdditiveSemigroup[A]): Unit = set(lhs + copyIfNeeded(rhs))

  def +=(rhs: A)(implicit A: AdditiveSemigroup[A]): Unit = this += ConstantVec(rhs, length)

  def -=(rhs: scalin.AbstractVec[A])(implicit A: AdditiveGroup[A]): Unit = set(lhs - copyIfNeeded(rhs))

  def -=(rhs: A)(implicit A: AdditiveGroup[A]): Unit = this -= ConstantVec(rhs, length)

  def :*=(rhs: scalin.AbstractVec[A])(implicit A: MultiplicativeSemigroup[A]): Unit = set(lhs :* copyIfNeeded(rhs))

  def :*=(rhs: A)(implicit A: MultiplicativeSemigroup[A]): Unit = this :*= ConstantVec(rhs, length)

  def *:=(realLhs: A)(implicit A: MultiplicativeSemigroup[A]): Unit = set(ConstantVec(realLhs, length) :* lhs)

  def :/=(rhs: scalin.AbstractVec[A])(implicit A: MultiplicativeGroup[A]): Unit = set(lhs :/ copyIfNeeded(rhs))

}
