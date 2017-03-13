package scalin
package impl
package builder

import spire.syntax.cfor._
import spire.syntax.euclideanRing._

import scalin.algebra.Pivot
import scalin.syntax.assign._

trait MatEuclideanRing[A, MA <: Mat[A]]
    extends scalin.impl.MatEuclideanRing[A, MA]
    with scalin.impl.builder.MatRing[A, MA] {

  implicit val pivotA: Pivot[A]

  implicit def UMA: scalin.algebra.MatEuclideanRing[A, UMA]
  implicit def UVA: scalin.algebra.VecEuclideanRing[A, UVA]

  /** Computes the rank of the matrix. */
  def rank(lhs: Mat[A]): Int = {
    import pivotA.closeToZero
    val ortho = orthogonalized(lhs)
      cforRange(ortho.nRows - 1 to 0 by -1) { r =>
        cforRange(0 until ortho.nCols) { c =>
          if (!closeToZero(ortho(r, c))) return r + 1
        }
      }
    0
  }

  def orthogonalized(lhs: Mat[A]) = {
    val res = UMA.fromMat(lhs)
    orthogonalize(res)
    result(res)
  }

  protected def orthogonalize(res: UMA): Unit = {
    import pivotA.closeToZero
    val zeroRows = scala.collection.mutable.BitSet.empty
    val nR = res.nRows
    val nC = res.nCols
    cforRange(0 until nR) { i =>
      if (!zeroRows.contains(i)) {
        cforRange(i + 1 until nR) { j =>
          if (!zeroRows.contains(j)) {
            var uv = scalar.zero
            var uu = scalar.zero
            cforRange(0 until nC) { c =>
              uv = uv + res(i, c) * res(j, c)
              uu = uu + res(i, c) * res(i, c)
            }
            var g = scalar.zero
            var rowIsZero = true
            cforRange(0 until nC) { c =>
              res(j, c) := uu * res(j, c) - uv * res(i, c)
              val rhs = res(j, c)
              if (!closeToZero(rhs)) rowIsZero = false
              g = if (closeToZero(g)) rhs
              else if (closeToZero(rhs)) g
              else if (pivotA.optionalExactEq.nonEmpty) scalar.gcd(rhs, g)(pivotA.optionalExactEq.get)
              else scalar.one
            }
            if (rowIsZero) zeroRows += j
            if (!closeToZero(g)) {
              cforRange(0 until nC) { c =>
                res(j, c) := res(j, c) /~ g
              }
            }
          }
        }
      }
    }
  }

}
