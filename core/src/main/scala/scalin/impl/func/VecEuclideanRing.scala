package scalin
package impl
package func

trait VecEuclideanRing[A, VA <: Vec[A]]
    extends scalin.impl.VecEuclideanRing[A, VA]
    with scalin.impl.func.VecRing[A, VA]
