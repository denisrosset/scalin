package scalin

import spire.algebra.Eq
import spire.syntax.EqOps

import org.scalacheck.Shrink
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate in Scalin tests (inspired by Cats).
  */
trait ScalinSuite extends FunSuite with Matchers
  with PropertyChecks
  with Discipline
  with StrictScalinEquality
  with spire.syntax.AllSyntax with spire.std.AnyInstances
  with scalin.syntax.AllSyntax
  with spire.syntax.GroupoidSyntax {

  // disable Eq syntax (by making `eqOps` not implicit), since it collides
  // with scalactic's equality
  override def eqOps[A:Eq](a:A): EqOps[A] = new EqOps[A](a)

  def discardEvaluation(): Nothing = throw new DiscardedEvaluationException

  def noShrink[T] = Shrink[T](_ => Stream.empty)

  override def convertToEqualizer[T](left: T): Equalizer[T] = ???

}
