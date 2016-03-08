package scalin
package impl
package func

trait VecField[A, VA <: Vec[A]]
    extends scalin.impl.VecField[A, VA]
    with scalin.impl.func.VecEuclideanRing[A, VA]
