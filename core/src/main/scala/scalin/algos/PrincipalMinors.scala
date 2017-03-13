package scalin
package algos

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.algebra._
import scalin.syntax.all._

object PrincipalMinors {

  /** Fins the principal minors of an n x n matrix in a field. */
  def apply[UMat <: mutable.Mat[A]:ClassTag, UVec <: mutable.Vec[A], A](mat: Mat[A])(implicit UMat: MatField[A, UMat], UVec: VecField[A, UVec], eqA: Eq[A]): UVec = {
    import UMat.scalar
    var a: UMat = mat.toMat[UMat]
    assert(a.nRows == a.nCols)
    val n = a.nRows
    val zeroPivs = collection.mutable.BitSet.empty
    val pm = zeros[A]((1 << n) - 1) // where the principal minors are stored
    var ipm = 0 // index for storing the principal minors
    var q = Array(a(::, ::)) // q is the input queue of unprocessed matrices, initial queue has 1 matrix to process
    cforRange(0 until n) { level =>
      val n1 = q(0).nRows
      val nq = q.length
      val qq = Array.fill[UMat](nq * 2)(null.asInstanceOf[UMat])
      var ipm1 = 0 // for indexing previous pm elements
      cforRange(0 until nq) { i =>
        a = q(i)
        pm(ipm) := a(0, 0)
        if (n1 > 1) {
          if (pm(ipm).isZero) {
            zeroPivs += ipm
            pm(ipm) := scalar.one
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

    for i = length(zeropivs):-1:1
    mask = uint64(zeropivs(i));
    delta = msb(mask);
    delta2 = 2*delta;
    ipm1 = bitand(uint64(mask), bitcmp(delta,'uint64'));
    if ipm1 == 0
    pm(mask) = pm(mask) - ppivot;
    else
      pm(mask) = (pm(mask)/pm(ipm1) - ppivot)*pm(ipm1);
    end
    for j = mask+delta2:delta2:2^n - 1
        pm(j) = pm(j) - ppivot*pm(j - delta);
    end
end

    ???
  }

}
