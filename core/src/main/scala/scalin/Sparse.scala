package scalin

import spire.algebra.{AdditiveGroup, AdditiveMonoid, Eq}

trait Sparse[A] {
  def zero: A
  def provenZero(a: A): Boolean
}

abstract class Sparse0 {
  implicit def fromAdditiveMonoidEq[A: Eq : AdditiveMonoid]: SparseAdditiveMonoid[A] = new SparseAdditiveMonoid[A] {
    def additive: AdditiveMonoid[A] = implicitly
    def zero: A = AdditiveMonoid[A].zero
    def provenZero(a: A): Boolean = AdditiveMonoid[A].isZero(a)
  }
}

object Sparse extends Sparse0 {
  def apply[A](implicit ev: Sparse[A]): Sparse[A] = ev
  implicit def fromAdditiveGroupEq[A:Eq:AdditiveGroup]: SparseAdditiveGroup[A] = new SparseAdditiveGroup[A] {
    def additive: AdditiveGroup[A] = implicitly
    def zero: A = AdditiveGroup[A].zero
    def provenZero(a: A): Boolean = AdditiveGroup[A].isZero(a)
  }
}

trait SparseAdditiveMonoid[A] extends Sparse[A] {
  def additive: AdditiveMonoid[A]
}

object SparseAdditiveMonoid {
  def apply[A](implicit ev: SparseAdditiveMonoid[A]): SparseAdditiveMonoid[A] = ev
}

trait SparseAdditiveGroup[A] extends SparseAdditiveMonoid[A] {
  def additive: AdditiveGroup[A]
}

object SparseAdditiveGroup {
  def apply[A](implicit ev: SparseAdditiveGroup[A]): SparseAdditiveGroup[A] = ev
}
