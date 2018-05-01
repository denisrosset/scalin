package scalin
package algos

import spire.algebra.{EuclideanRing, Field, NRoot, Ring}
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.syntax.assign._

object GramSchmidt {

  def euclideanRing[A:EuclideanRing:mutable.MatEngine:Pivot](res: mutable.Mat[A]): Unit = {
    val pivotA = Pivot[A]
    import pivotA.closeToZero
    val zeroRows = scala.collection.mutable.BitSet.empty
    val nR = res.nRows
    val nC = res.nCols
    cforRange(0 until nR) { i =>
      if (!zeroRows.contains(i)) {
        cforRange(i + 1 until nR) { j =>
          if (!zeroRows.contains(j)) {
            var uv = Ring[A].zero
            var uu = Ring[A].zero
            cforRange(0 until nC) { c =>
              uv = uv + res(i, c) * res(j, c)
              uu = uu + res(i, c) * res(i, c)
            }
            var g = Ring[A].zero
            var rowIsZero = true
            cforRange(0 until nC) { c =>
              res(j, c) := uu * res(j, c) - uv * res(i, c)
              val rhs = res(j, c)
              if (!closeToZero(rhs)) rowIsZero = false
              g = if (closeToZero(g)) rhs
              else if (closeToZero(rhs)) g
              else if (pivotA.optionalExactEq.nonEmpty) EuclideanRing[A].gcd(rhs, g)(pivotA.optionalExactEq.get)
              else Ring[A].one
            }
            if (rowIsZero) zeroRows += j
            if (!closeToZero(g)) {
              cforRange(0 until nC) { c =>
                res(j, c) := res(j, c) equot g
              }
            }
          }
        }
      }
    }
  }

  def field[A:Field:mutable.MatEngine:Pivot](m: mutable.Mat[A]): Unit = {
    val pivotA = Pivot[A]
    import pivotA.closeToZero
    val nR = m.nRows
    val nC = m.nCols
    cforRange(0 until nR) { i =>
      cforRange(i + 1 until nR) { j =>
        var uv: A = Ring[A].zero
        var uu: A = Ring[A].zero
        cforRange(0 until nC) { c =>
          uv = uv + m(i, c) * m(j, c)
          uu = uu + m(i, c) * m(i, c)
        }
        if (!closeToZero(uu)) {
          val factor = uv / uu
          cforRange(0 until nC) { c =>
            m(j, c) := m(j, c) - factor * m(i, c)
          }
        }
      }
    }

  }

  protected def normalize[A:Field:mutable.MatEngine:NRoot](m: mutable.Mat[A]): Unit = {
    cforRange(0 until m.nRows) { r =>
      var norm2: A = Ring[A].zero
      cforRange(0 until m.nCols) { c =>
        norm2 = norm2 + m(r, c) * m(r, c)
      }
      val normInv = spire.math.sqrt(norm2).reciprocal
      cforRange(0 until m.nCols) { c =>
        m(r, c) := m(r, c) * normInv
      }
    }
  }

  def orthonormalize[A:Field:mutable.MatEngine:NRoot:Pivot](m: mutable.Mat[A]): Unit = {
    field(m)
    normalize(m)
  }

}
