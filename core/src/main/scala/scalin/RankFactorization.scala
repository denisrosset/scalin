package scalin

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
