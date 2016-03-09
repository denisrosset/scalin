package scalin
package impl
package builder

trait VecField[A, VA <: Vec[A]]
    extends scalin.impl.VecField[A, VA]
    with scalin.impl.builder.VecEuclideanRing[A, VA]
