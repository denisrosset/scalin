package scalin

import spire.algebra.Rng
import spire.math.{Complex, Rational}

trait Involution[A] {

  def dagger(a: A): A

}

object Involution {

  implicit object int extends Involution[Int] {
    def dagger(a: Int) = a
  }

  implicit object long extends Involution[Long] {
    def dagger(a: Long) = a
  }

  implicit object float extends Involution[Float] {
    def dagger(a: Float) = a
  }

  implicit object double extends Involution[Double] {
    def dagger(a: Double) = a
  }

  implicit object rational extends Involution[Rational] {
    def dagger(a: Rational) = a
  }

  implicit def complex[A:Rng]: Involution[Complex[A]] = new Involution[Complex[A]] {
    def dagger(a: Complex[A]) = a.conjugate
  }

}
