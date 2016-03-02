package scalin

import spire.algebra.Rng
import spire.math.{Complex, Rational}

trait Transposition[A] {

  def transpose(a: A): A

}

object Transposition {

  implicit object int extends Transposition[Int] {
    def transpose(a: Int) = a
  }

  implicit object long extends Transposition[Long] {
    def transpose(a: Long) = a
  }

  implicit object float extends Transposition[Float] {
    def transpose(a: Float) = a
  }

  implicit object double extends Transposition[Double] {
    def transpose(a: Double) = a
  }

  implicit object rational extends Transposition[Rational] {
    def transpose(a: Rational) = a
  }

  implicit def complex[A:Rng]: Transposition[Complex[A]] = new Transposition[Complex[A]] {
    def transpose(a: Complex[A]) = a
  }

}
