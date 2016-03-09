package scalin
package impl
package builder

import scalin.syntax.assign._

import spire.syntax.cfor._
import spire.syntax.multiplicativeMonoid._

trait MatMultiplicativeMonoid[A, MA <: Mat[A], UA <: mutable.Mat[A]]
    extends scalin.impl.MatMultiplicativeMonoid[A, MA]
    with scalin.impl.builder.MatEngine[A, MA, UA] {

  implicit def UA: scalin.algebra.MatMultiplicativeMonoid[A, UA]

  def kron(x: Mat[A], y: Mat[A]): MA = {
    val nrx = x.rows
    val ncx = x.cols
    val nry = y.rows
    val ncy = y.cols
    val nR = nrx * nry
    val nC = ncx * ncy
    val b = alloc(nR, nC)
    var r = 0
    cforRange(0 until nrx) { rx =>
      cforRange(0 until nry) { ry =>
        var c = 0
        cforRange(0 until ncx) { cx =>
          cforRange(0 until ncy) { cy =>
            b(r, c) := x(rx, cx) * y(ry, cy)
            c += 1
          }
        }
        r += 1
      }
    }
    result(b)
  }

}
