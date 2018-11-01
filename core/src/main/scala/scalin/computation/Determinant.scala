package scalin
package computation

import spire.algebra.Ring
import spire.syntax.cfor._
import spire.syntax.ring._
import scalin.syntax.all._

/** Computes the matrix determinant. Requires a square matrix. */
object Determinant {

  /** Matrix-in-ring determinant algorithm from Mahajan and Vinay, see
    * http://cjtcs.cs.uchicago.edu/articles/1997/5/cj97-05.pdf
    * 
    * TODO: implement optimizations present in
    * https://github.com/gap-system/gap/blob/master/lib/matrix.gi
    * 
    * TODO: check if valid for noncommutative rings
    */
  def ring[A:Ring](lhs: Mat[A]): A = {
    import scalin.mutable.dense._
    val n = lhs.nRows
    require(lhs.nCols == n)
    var current = new Array[mutable.Mat[A]](2)
    val b = n % 2
    current(b) = mutable.Mat.eye[A](n)
    current(1 - b) = mutable.Mat.zeros[A](n, n)
    var next = Array[mutable.Mat[A]](mutable.Mat.zeros[A](n, n), mutable.Mat.zeros[A](n, n))
    cforRange(0 to n - 2) { i =>
      cforRange(0 until n) { v =>
        cforRange(0 to v) { u =>
          cforRange(0 to 1) { p =>
            cforRange(u + 1 until n) { w =>
              next(p)(u,w) := next(p)(u, w) + current(p)(u, v) * lhs(v, w)
              next(1-p)(w,w) := next(1-p)(w,w) + current(p)(u,v) * lhs(v,u)
            }
          }
        }
      }
      val temp = current
      current = next
      temp(0)(::, ::) := Ring[A].zero
      temp(1)(::, ::) := Ring[A].zero
      next = temp
    }
    var tplus = Ring[A].zero
    var tminus = Ring[A].zero
    cforRange(0 until n) { v =>
      cforRange(0 to v) { u =>
        tplus += current(1)(u, v) * lhs(v, u)
        tminus += current(0)(u, v) * lhs(v, u)
      }
    }
    tplus - tminus
  }

}
