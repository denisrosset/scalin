package scalin
package algorithms

/** Reduced row echelon form of a matrix (Gauss-Jordan elimination). */
trait RankFactorization[A] { lhs =>

  type MA <: Mat[A]

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
  def rref: MA

  def matC(original: Mat[A]): MA

  def matF: MA

}

object RankFactorization {

  /* 
    def inplaceRankFactorization(m: UMA): RankFactorization[A] { type MA = UMA } = {
    val used = collection.mutable.ArrayBuilder.make[Int]
    var r = 0
    cforRange(0 until m.nCols) { c =>
      if (r < m.nRows) {
        var priority = pivotA.priority(m(r, c))
        var pivot = r
        cforRange((r + 1) until m.nRows) { r1 =>
          val r1Priority = pivotA.priority(m(r1, c))
          if (r1Priority > priority) {
            priority = r1Priority
            pivot = r1
          }
        }
        if (priority != 0) { // if el is zero, skip the column c
          used += c // keep track of bound variables

          // swap current row and pivot row
          cforRange(c until m.nCols) { c1 =>
            val tmp = m(pivot, c1)
            m(pivot, c1) := m(r, c1)
            m(r, c1) := tmp
          }
          // normalize pivot row
          val f = m(r, c)
          cforRange(c until m.nCols) { c1 =>
            m(r, c1) := m(r, c1) / f
          }
          // eliminate current column
          cforRange(0 until m.nRows) { r1 =>
            if (r1 != r) {
              val g = m(r1, c)
              cforRange(c until m.nCols) { c1 =>
                m(r1, c1) := m(r1, c1) - g * m(r, c1)
              }
            }
          }
          r += 1
        } else // set zero terms to exact zero (used for floating point)
          cforRange(r until m.nRows) { r1 =>
            m(r, c) := scalar.zero
          }
      }
    }
    new RankFactorizationImpl(m, used.result)
  }

  final class RankFactorizationImpl(val rref: UMA, basis: Array[Int]) extends RankFactorization[A] {

    def rank = basis.length

    def basisIndex(k: Int) = basis(k)

    type MA = UMA

    def matC(original: Mat[A]): UMA = UMA.slice(original, ::, basis: Subscript)

    def matF: UMA = UMA.slice(rref, 0 until rank, ::)

  }
   */
}

