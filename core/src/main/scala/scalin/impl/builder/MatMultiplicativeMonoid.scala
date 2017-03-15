package scalin
package impl
package builder

import scalin.syntax.assign._

import spire.syntax.cfor._
import spire.syntax.multiplicativeMonoid._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]]
    extends scalin.impl.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.builder.MatEngine[A, MA]

