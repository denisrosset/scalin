package scalin
package impl
package builder

trait VecEuclideanRing[A, VA <: Vec[A], UA <: mutable.Vec[A]]
    extends scalin.impl.VecEuclideanRing[A, VA]
    with scalin.impl.builder.VecRing[A, VA, UA]
