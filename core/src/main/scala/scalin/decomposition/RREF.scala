package scalin.decomposition

import scalin.syntax.all._
import scalin.{Mat, Pivot, Subscript}
import spire.algebra.Field
import spire.syntax.cfor._
import spire.syntax.field._

/** Reduced row echelon form of a matrix (Gauss-Jordan elimination).
  * Also provides the rank factorization A = C * F */
trait RREF[A] { lhs =>

  /** Rank of the original matrix. */
  def rank: Int

  /** For k = 0 .. rank - 1, return an index such that
    * `mat(:,basisIndex(0 .. rank-1))` is a basis for the range of `mat`.
    * 
    * `basisIndex(0 .. rank-1)` are also pivot variables in a linear system
    * `mat * x = b`.
    */
  def basisIndex(k: Int): Int

  /** Matrix in reduced row echelon form, such that 
    * `reduced(0 .. rank-1, basisIndex(0 .. rank-1))` is the identity matrix.
    */
  def rref: scalin.immutable.Mat[A]

  def C: scalin.immutable.Mat[A]

  def F: scalin.immutable.Mat[A]

  def value(implicit A: Field[A], ev: scalin.immutable.MatEngine[A]): Mat[A] = C * F
}

class RREFImpl[A](val A: scalin.immutable.Mat[A], val rref: scalin.immutable.Mat[A], basis: Array[Int]) extends RREF[A] {
  import scalin.immutable.dense._

  def rank: Int = basis.length

  def basisIndex(k: Int): Int = basis(k)

  def C: scalin.immutable.Mat[A] = A(::, basis: Subscript)

  def F: scalin.immutable.Mat[A] = rref(0 until rank, ::)
}

object RREF {

  def apply[A: Field : Pivot](A: scalin.immutable.Mat[A]): RREF[A] = {
    import scalin.immutable.dense._
    val used = collection.mutable.ArrayBuilder.make[Int]
    val rref = scalin.immutable.Mat.fromMutable(A) { mut =>
      var r = 0
      cforRange(0 until mut.nCols) { c =>
        if (r < mut.nRows) {
          var priority = Pivot[A].priority(mut(r, c))
          var pivot = r
          cforRange((r + 1) until mut.nRows) { r1 =>
            val r1Priority = Pivot[A].priority(mut(r1, c))
            if (r1Priority > priority) {
              priority = r1Priority
              pivot = r1
            }
          }
          if (priority != 0) { // if el is zero, skip the column c
            used += c // keep track of bound variables

            // swap current row and pivot row
            cforRange(c until mut.nCols) { c1 =>
              val tmp = mut(pivot, c1)
              mut(pivot, c1) := mut(r, c1)
              mut(r, c1) := tmp
            }
            // normalize pivot row
            val f = mut(r, c)
            cforRange(c until mut.nCols) { c1 =>
              mut(r, c1) := mut(r, c1) / f
            }
            // eliminate current column
            cforRange(0 until mut.nRows) { r1 =>
              if (r1 != r) {
                val g = mut(r1, c)
                cforRange(c until mut.nCols) { c1 =>
                  mut(r1, c1) := mut(r1, c1) - g * mut(r, c1)
                }
              }
            }
            r += 1
          } else // set zero terms to exact zero (used for floating point)
            cforRange(r until mut.nRows) { r1 =>
              mut(r, c) := Field[A].zero
            }
        }
      }
    }
    new RREFImpl[A](A, rref, used.result)
  }

}
