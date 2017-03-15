package scalin

import spire.algebra.{AdditiveMonoid, Eq}

trait Sparse[A] {

  def zero: A

  def provenZero(a: A): Boolean

}

object Sparse {

  implicit def fromAdditiveMonoidEq[A:Eq:AdditiveMonoid]: Sparse[A] = new Sparse[A] {

    def zero = AdditiveMonoid[A].zero

    def provenZero(a: A) = AdditiveMonoid[A].isZero(a)

  }

}
