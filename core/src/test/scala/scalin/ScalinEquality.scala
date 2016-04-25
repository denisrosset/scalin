package scalin

import spire.algebra.Eq

import org.scalactic.TripleEqualsSupport.{AToBEquivalenceConstraint, BToAEquivalenceConstraint}
import org.scalactic._

// The code in this file was taken from Cats, which is turn contains
// the following comment:

// The code in this file was taken and only slightly modified from
// https://github.com/bvenners/equality-integration-demo
// Thanks for the great examples, Bill!

final class ScalinEquivalence[T](T: Eq[T]) extends Equivalence[T] {

  def areEquivalent(a: T, b: T): Boolean = T.eqv(a, b)

}

trait LowPriorityStrictScalinConstraints extends TripleEquals {

  implicit def lowPriorityCatsCanEqual[A, B](implicit B: Eq[B], ev: A <:< B): CanEqual[A, B] =
    new AToBEquivalenceConstraint[A, B](new ScalinEquivalence(B), ev)

}

trait StrictScalinEquality extends LowPriorityStrictScalinConstraints {

  override def convertToEqualizer[T](left: T): Equalizer[T] = super.convertToEqualizer[T](left)

  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): CanEqual[A, B] = super.unconstrainedEquality[A, B]

  implicit def ScalinCanEqual[A, B](implicit A: Eq[A], ev: B <:< A): CanEqual[A, B] =
    new BToAEquivalenceConstraint[A, B](new ScalinEquivalence(A), ev)

}

object StrictScalinEquality extends StrictScalinEquality
