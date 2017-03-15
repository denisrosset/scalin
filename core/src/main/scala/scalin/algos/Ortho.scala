package scalin
package algos

import spire.algebra.{EuclideanRing, Field}
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.algebra.Pivot
import scalin.syntax.assign._

object Orthogonal {
/*
  def orthogonalized[A:EuclideanRing:Pivot, MA <: Mat[A]](lhs: Mat[A])(implicit MA: algebra.MatEuclideanRing[A, MA]) =
    MA.fromMutable(lhs) { res =>
      import mutable.dense._
      Ortho.orthogonalize[A, mutable.Mat[A]](res)
    }*/

  def euclideanRing[A, MA <: mutable.Mat[A]](res: MA)(implicit scalar: EuclideanRing[A], MA: algebra.MatEuclideanRing[A, MA], pivotA: Pivot[A]): Unit = {
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

  def field[A, MA <: mutable.Mat[A]](m: MA)(implicit scalar: Field[A], MA: algebra.MatField[A, MA], pivotA: Pivot[A]): Unit = {
    import pivotA.closeToZero
    val nR = m.nRows
    val nC = m.nCols
    cforRange(0 until nR) { i =>
      cforRange(i + 1 until nR) { j =>
        var uv: A = scalar.zero
        var uu: A = scalar.zero
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

}

object Orthonormal {

  /*
  protected def normalize(m: UMA)(implicit nroot: NRoot[A]): Unit = {
    cforRange(0 until m.nRows) { r =>
      var norm2: A = scalar.zero
      cforRange(0 until m.nCols) { c =>
        norm2 = norm2 + m(r, c) * m(r, c)
      }
      val normInv = spire.math.sqrt(norm2).reciprocal
      cforRange(0 until m.nCols) { c =>
        m(r, c) := m(r, c) * normInv
      }
    }    
  }

  def orthonormalized(lhs: Mat[A])(implicit nroot: NRoot[A]): MA = {
    val res = UMA.fromMat(lhs)
    orthogonalize(res)
    normalize(res)
    result(res)
  }
   */
}

