package scalin
package impl
package builder

import spire.algebra.MultiplicativeMonoid
import spire.syntax.cfor._
import spire.syntax.multiplicativeMonoid._

import scalin.syntax.assign._

trait VecMultiplicativeMonoid[A, VA <: Vec[A], UA <: mutable.Vec[A]]
    extends scalin.impl.VecMultiplicativeMonoid[A, VA]
    with scalin.impl.builder.VecFactory[A, VA, UA] {

  //// With `MultiplicativeMonoid[A]`, returning vector

  def kron(x: Vec[A], y: Vec[A]): VA = {
    val nx = x.length
    val ny = y.length
    var i = 0
    val b = alloc(nx * ny)
    cforRange(0 until nx) { ix =>
      cforRange(0 until ny) { iy =>
        b(i) := x(ix) * y(iy)
        i += 1
      }
    }
    result(b)
  }

}
