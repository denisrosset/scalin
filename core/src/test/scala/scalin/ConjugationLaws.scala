package scalin

import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Ring}
import org.scalacheck.Prop.forAll
import spire.math.{Complex, Quaternion, Rational}
import spire.syntax.eq._

object ConjugationLaws {
  def apply[A: Eq: Arbitrary] = new ConjugationLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait ConjugationLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def conjugation(implicit A: Conjugation[A]) = new DefaultRuleSet(
    name = "conjugation",
    parent = None,
    "involution" -> forAll( (x: A) => A.conjugate(A.conjugate(x)) === x )
  )

  def conjugationRing(implicit A: Conjugation[A], ringA: Ring[A]) = new DefaultRuleSet(
    name = "conjugationRing",
    parent = Some(conjugation),
    "preserves one" -> (A.conjugate(ringA.one) === ringA.one),
    "compatible with addition" -> forAll( (x: A, y: A) => A.conjugate(ringA.plus(x, y)) === ringA.plus(A.conjugate(x), A.conjugate(y))),
    "antiautomorphism" -> forAll( (x: A, y: A) => A.conjugate(ringA.times(x, y)) === ringA.times(A.conjugate(y), A.conjugate(x)))
  )
}

class ConjugationTests extends ScalinSuite {
  import spire.laws.arb._
  checkAll("Conjugation[Complex[Rational]]", ConjugationLaws[Complex[Rational]].conjugationRing)
  checkAll("Conjugation[Quaternion[Rational]]", ConjugationLaws[Quaternion[Rational]].conjugationRing)
}
