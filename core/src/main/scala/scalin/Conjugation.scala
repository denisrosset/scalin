package scalin

import spire.algebra.Rng
import spire.math.{Complex, Quaternion, Rational, SafeLong}

/** Describes a conjugation, which is an antihomomorphism. On a ring, it obeys:
  *
  *   i. conj(1) = 1
  *  ii. conj(x + y) = conj(x) + conj(y)
  * iii. conj(x*y) = conj(y)*conj(x)
  */
trait Conjugation[A] {

  def conjugate(a: A): A
}

object Conjugation {

  def apply[A](implicit ev: Conjugation[A]): Conjugation[A] = ev

  implicit def complex[A:Rng]: Conjugation[Complex[A]] = new Conjugation[Complex[A]] {
    def conjugate(a: Complex[A]): Complex[A] = a.conjugate
  }

  implicit def quaternion[A:Rng]: Conjugation[Quaternion[A]] = new Conjugation[Quaternion[A]] {
    def conjugate(a: Quaternion[A]): Quaternion[A] = a.conjugate
  }

  def id[A]: Conjugation[A] = new Conjugation[A] {
    def conjugate(a: A): A = a
  }

  implicit val rational: Conjugation[Rational] = id[Rational]
  implicit val safeLong: Conjugation[SafeLong] = id[SafeLong]
}
