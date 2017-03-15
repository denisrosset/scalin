package scalin
package impl

import spire.algebra.Eq
import spire.syntax.cfor._
import spire.syntax.eq._

trait MatEngine[A, MA <: Mat[A]] extends scalin.algebra.MatEngine[A, MA]

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.algebra.MatEuclideanRing[A, MA]
    with scalin.impl.MatRing[A, MA]

trait MatField[A, MA <: Mat[A]]
    extends scalin.algebra.MatField[A, MA]
    with scalin.impl.MatEuclideanRing[A, MA]

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.algebra.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.MatEngine[A, MA]

trait MatRing[A, MA <: Mat[A]]
    extends scalin.algebra.MatRing[A, MA]
    with scalin.impl.MatMultiplicativeMonoid[A, MA]
