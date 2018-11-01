package scalin
package computation

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.syntax.assign._

object PrincipalMinors {

  // Based on the algorithm of Griffin&Tsatsomeros doi:10.1016/j.laa.2006.04.008

  /** Finds the principal minors of an n x n matrix in a field. */
  def apply[A:Eq:Field:mutable.VecEngine:mutable.MatEngine](mat: Mat[A]): mutable.Vec[A] = {
    var a: mutable.Mat[A] = mat.to[mutable.Mat[A]](MatConv.fromEngine)
    assert(a.nRows == a.nCols)
    val n = a.nRows
    val zeroPivs = collection.mutable.BitSet.empty
    val pm = implicitly[mutable.VecEngine[A]].zeros((1 << n) - 1) // where the principal minors are stored
    var ipm = 0 // index for storing the principal minors
    var q = collection.mutable.Seq(a(::, ::)) // q is the input queue of unprocessed matrices, initial queue has 1 matrix to process
    cforRange(0 until n) { level =>
      var ipm1 = 0 // for indexing previous pm elements
      val n1 = q(0).nRows
      val nq = q.length
      val qq = collection.mutable.Seq.fill[mutable.Mat[A]](nq * 2)(null.asInstanceOf[mutable.Mat[A]])
      cforRange(0 until nq) { i =>
        a = q(i)
        pm(ipm) := a(0, 0)
        if (n1 > 1) {
          if (pm(ipm).isZero) {
            zeroPivs += ipm
            pm(ipm) := Field[A].one
          }
          val b = a(1 until n1, 1 until n1)
          val d = a(1 until n1, 0)/pm(ipm)
          val c = b - (d dyad a(0, 1 until n1))

          // Order the output queue to make the elements of pm come out in the correct order.
          qq(i) = b
          qq(i + nq) = c
        }
        if (i > 0) {
          // if i > 1, to convert from a general pivot to a principal
          // minor, we need to multiply by every element of the pm matrix
          // we have already generated, in the order that we generated it.
          pm(ipm) := pm(ipm) * pm(ipm1)
          ipm1 += 1
        }
        ipm += 1
      }
      q = qq
    }

    // Zero Pivot Loop
    //
    // Now correct principal minors for all places we used 1 as a pivot
    // in place of a 0.

    while (zeroPivs.nonEmpty) {
      val mask = zeroPivs.max + 1 // matlab is one-based, so we shift upwards
      val delta = java.lang.Integer.highestOneBit(mask)
      val delta2 = 2 * delta
      val ipm1 = (~delta) & mask
      if (ipm1 == 0)
        pm(mask - 1) := pm(mask - 1) - Field[A].one // but we shift downwards on access
      else
        pm(mask - 1) := (pm(mask - 1)/pm(ipm1 - 1) - Field[A].one) * pm(ipm1 - 1)
      (mask + delta to ((1 << n) - 1) by delta2).foreach { j =>
        pm(j - 1) := pm(j - 1) - pm(j - delta - 1)
      }
    }

    pm
  }

}
