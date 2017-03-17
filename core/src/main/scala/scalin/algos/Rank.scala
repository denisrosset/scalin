package scalin
package algos

/*import spire.algebra.EuclideanRing
import spire.syntax.cfor._
import spire.syntax.euclideanRing._

import scalin.syntax.assign._*/

object Rank {
/* TODO
  /** Computes the rank of the matrix. */
  def rank[A](lhs: Mat[A]): Int = {
    import mutable.dense._
    import pivotA.closeToZero
    val ortho = orthogonalized(lhs)
      cforRange(ortho.nRows - 1 to 0 by -1) { r =>
        cforRange(0 until ortho.nCols) { c =>
          if (!closeToZero(ortho(r, c))) return r + 1
        }
      }
    0
  }
 */
}
