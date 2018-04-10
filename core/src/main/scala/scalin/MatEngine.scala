package scalin

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.field._
import scalin.syntax.assign._
import spire.util.Opt

import scala.annotation.tailrec

/** Builder for matrices with an arbitrary scalar type `A`. */
trait MatEngine[A, +MA <: Mat[A]] { self =>

  type Ret <: MA // hack for the return type of Mat.flatten

  implicit def MA: MatEngine[A, MA] = self

  //// Construct this from mutable matrices

  type Mut <: scalin.mutable.Mat[A]
  def mutableEngine: MatEngine[A, Mut]
  implicit def mutableConv: MatConv[A, Mut, MA]

  //// Minimal methods to implement

  /** Creates a matrix from the given size (nRows, nCols) and a value function. */
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): MA

  /** Creates a matrix from the given blocks. */
  def blockTabulate(nBlockRows: Int, nBlockCols: Int)(f: (Int, Int) => Mat[A]): MA = {
    var nonEmpty: Opt[Mat[A]] = Opt.empty[Mat[A]]
    val array = new Array[Mat[A]](nBlockRows * nBlockCols)
    cforRange(0 until nBlockRows) { br =>
      cforRange(0 until nBlockCols) { bc =>
        val block = f(br, bc)
        array(br + bc * nBlockRows) = block
        if (block.nRows != 0 && block.nCols != 0)
          nonEmpty = Opt(block)
      }
    }
    @tailrec def computeRows(s: Int, br: Int): Int =
      if (br == nBlockRows) s else computeRows(s + array(br).nRows, br + 1)
    @tailrec def computeCols(s: Int, bc: Int): Int =
      if (bc == nBlockCols) s else computeCols(s + array(bc * nBlockRows).nCols, bc + 1)
    val nRows = computeRows(0, 0)
    val nCols = computeCols(0, 0)
    val mut = nonEmpty match {
      case Opt(block) => mutableEngine.fillConstant(nRows, nCols)(block(0, 0))
      case _ => return tabulate(nRows, nCols)( (r, c) => sys.error("Matrix is empty") )
    }
    var colStart = 0
    cforRange(0 until nBlockCols) { bc =>
      var rowStart = 0
      cforRange(0 until nBlockRows) { br =>
        val block = array(br + bc * nBlockRows)
        mut(rowStart until rowStart + block.nRows, colStart until colStart + block.nCols) := block
        rowStart += block.nRows
        if (br == nBlockRows - 1)
          colStart += block.nCols
      }
    }
    mutableConv(mut)
  }

  /** Creates a nRows x nCols matrix with the specific element `fill`.
    * The fill is only evaluated once and copied in the whole matrix. */
  def fillConstant(nRows: Int, nCols: Int)(a: => A): MA

  /** Builds a matrix from the given size and a user-provided function that mutates
    * a temporary mutable matrix previously filled with the provided `default` value.
    *
    * default is evaluated once if the matrix has > 0 elements,
    * and then the result is copied over the matrix.
    */
  def fromMutable(nRows: Int, nCols: Int, default: => A)(updateFun: Mut => Unit): MA =
    if (nRows == 0 || nCols == 0)
      tabulate(nRows, nCols)(sys.error("Never used"))
    else {
      val mutable = mutableEngine.fillConstant(nRows, nCols)(default)
      updateFun(mutable)
      mutableConv(mutable)
    }

  /** Builds a matrix from the processing applied on a mutable copy of the provided matrix. */
  def fromMutable(mat: Mat[A])(updateFun: Mut => Unit): MA =
    fromMutable(mat.nRows, mat.nCols, mat(0, 0)) { res =>
      res(::, ::) := mat
      updateFun(res)
    }

  //// Helper methods

  def pointwiseUnary(lhs: Mat[A])(f: A => A) = tabulate(lhs.nRows, lhs.nCols)((r, c) => f(lhs(r, c)) )

  def pointwiseBinary(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => A): MA = {
    require(lhs.nRows == rhs.nRows)
    require(lhs.nCols == rhs.nCols)
    tabulate(lhs.nRows, lhs.nCols)((r, c) => f(lhs(r, c), rhs(r, c)) )
  }

  def booleanBinaryAnd(lhs: Mat[A], rhs: Mat[A])(f: (A, A) => Boolean): Boolean =
    (lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols) && {
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          if (!f(lhs(r, c), rhs(r, c))) return false
        }
      }
      true
    }

  def pointwiseBooleanUnary[B](lhs: Mat[B])(f: B => Boolean)(implicit ev: Boolean =:= A): MA =
    tabulate(lhs.nRows, lhs.nCols)((r, c) =>  f(lhs(r, c)) )

  def pointwiseBooleanBinary[B](lhs: Mat[B], rhs: Mat[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): MA = {
    require(lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols)
    tabulate(lhs.nRows, lhs.nCols)((r, c) =>  f(lhs(r, c), rhs(r, c)) )
  }

  //// Creation

  // empty matrix is an ill-defined object (0x0, nx0 and 0xn are all empty)

  def sparse(nRows: Int, nCols: Int)(i: Vec[Int], j: Vec[Int], v: Vec[A])(implicit A: AdditiveMonoid[A], sparse: Sparse[A]): MA =
    fromMutable(nRows, nCols, sparse.zero) { res =>
      require(i.length == j.length)
      require(i.length == v.length)
      cforRange(0 until i.length) { k =>
        res(i(k), j(k)) := res(i(k), j(k)) + v(k)
      }
    }

  def fill(nRows: Int, nCols: Int)(a: => A): MA = tabulate(nRows, nCols)( (r, c) => a )

  def colMajor(rows: Int, cols: Int)(elements: A*): MA = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(r + c * rows) )
  }

  def rowMajor(rows: Int, cols: Int)(elements: A*): MA = {
    require(elements.size == rows * cols)
    tabulate(rows, cols)( (r, c) => elements(c + r * cols) )
  }

  def rowMat(elements: A*): MA = rowMajor(1, elements.size)(elements: _*)

  def colMat(elements: A*): MA = colMajor(elements.size, 1)(elements: _*)

  def toRowMat(lhs: Vec[A]): MA = tabulate(1, lhs.length)( (r, c) => lhs(c) )

  def toColMat(lhs: Vec[A]): MA = tabulate(lhs.length, 1)( (r, c) => lhs(r) )

  def fromMat(mat: Mat[A]): MA = tabulate(mat.nRows, mat.nCols)((r, c) => mat(r, c) )

  //// Collection-like methods

  /* Alternative

  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      def flatRow(r: Int): MA = {
        if (lhs.nCols == 1) map(f(lhs(r, 0)))(identity)
        else {
          var accRow = horzcat(f(lhs(r, 0)), f(lhs(r, 1)))
          cforRange(2 until lhs.nCols) { c =>
            accRow = horzcat(accRow, f(lhs(r, c)))
          }
          accRow
        }
      }
      if (lhs.nRows == 1) flatRow(0)
      else {
        var acc = vertcat(flatRow(0), flatRow(1))
        cforRange(2 until lhs.nRows) { r =>
          acc = vertcat(acc, flatRow(r))
        }
        acc
      }
    }
   */
  /** Returns the flattened block matrix specified by `lhs.map(f)`. Not defined if the matrix is empty. */
  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else blockTabulate(lhs.nRows, lhs.nCols)( (r, c) => f(lhs(r, c)) )

/* Alternative
  def flatten[B <: Mat[A]](lhs: Mat[B]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else {
      def flatRow(r: Int): MA = {
        if (lhs.nCols == 1) map(lhs(r, 0))(identity)
        else {
          var accRow = horzcat(lhs(r, 0), lhs(r, 1))
          cforRange(2 until lhs.nCols) { c =>
            accRow = horzcat(accRow, lhs(r, c))
          }
          accRow
        }
      }
      if (lhs.nRows == 1) flatRow(0)
      else {
        var acc = vertcat(flatRow(0), flatRow(1))
        cforRange(2 until lhs.nRows) { r =>
          acc = vertcat(acc, flatRow(r))
        }
        acc
      }
    }
 */

  /** Flatten a block matrix. Not defined if the matrix is empty. */
  def flatten[B <: Mat[A]](lhs: Mat[B]): MA =
    if (lhs.nRows == 0 || lhs.nCols == 0) sys.error("Cannot flatten matrix with 0 rows or zero cols.")
    else blockTabulate(lhs.nRows, lhs.nCols)( (r, c) => lhs(r, c) )

  /** Builds a new matrix by applying a function to all elements of this matrix. */
  def map[B](lhs: Mat[B])(f: B => A): MA = tabulate(lhs.nRows, lhs.nCols)((r, c) => f(lhs(r, c)) )

  /* Alternative
  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val m = lhs.nRows
    require(m == rhs.nRows)
    val nl = lhs.nCols
    val nr = rhs.nCols
    tabulate(m, nl + nr)( (r, c) => if (c < nl) lhs(r, c) else rhs(r, c - nl) )
  }
   */
  /** Returns the horizontal concatenation of two matrices with the same number of rows. */
  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA =
    blockTabulate(1, 2)( (r, c) => if (c == 0) lhs else rhs )

  /* Alternative
  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val n = lhs.nCols
    require(n == rhs.nCols)
    val ml = lhs.nRows
    val mr = rhs.nRows
    tabulate(ml + mr, n)( (r, c) => if (r < ml) lhs(r, c) else rhs(r - ml, c) )
  }
   */
  /** Returns the vertical concatenation of two matrices with the same number of columns. */
  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA =
    blockTabulate(2, 1)( (r, c) => if (r == 0) lhs else rhs )

  //// Slices

  /** Returns a matrix slice of a matrix.
    * The return value is a copy (i.e. not read- or write-through as in scala.breeze). */
  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): MA = {
    val ri = rs.forLength(mat.nRows)
    val ci = cs.forLength(mat.nCols)
    tabulate(ri.length, ci.length)( (k, l) => mat(ri(k), ci(l)) )
  }

  //// Shuffling elements around

  /** Returns the matrix transpose. Does not conjuagates complex numbers. */
  def t(mat: Mat[A]): MA = tabulate(mat.nCols, mat.nRows)((i, j) => mat(j, i) )

  /** Reshapes a vector in a matrix shape, using column-major ordering of elements. */ 
  def reshape(vec: Vec[A], rows1: Int, cols1: Int): MA = {
    require(vec.length == rows1 * cols1)
    tabulate(rows1, cols1)( (r1, c1) => vec(r1 + c1 * rows1) )
  }

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  //// With `Eq[A]`

  def eqv(lhs: Mat[A], rhs: Mat[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

  //// Creation

  def ones(rows: Int, cols: Int)(implicit A: MultiplicativeMonoid[A]): MA = fillConstant(rows, cols)(A.one)

  //// With `MultiplicativeMonoid[A]`, returning matrix

  /** Scalar-matrix product. */
  def times(lhs: A, rhs: Mat[A])(implicit A: MultiplicativeSemigroup[A]): MA = pointwiseUnary(rhs)(A.times(lhs, _))

  /** Matrix-scalar product. */
  def times(lhs: Mat[A], rhs: A)(implicit A: MultiplicativeSemigroup[A]): MA = pointwiseUnary(lhs)(A.times(_, rhs))

  /** Pointwise multiplication, i.e. Hadamard product, see https://en.wikipedia.org/wiki/Hadamard_product_%28matrices%29 . */
  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A])(implicit A: MultiplicativeSemigroup[A]): MA = pointwiseBinary(lhs, rhs)(A.times)

  /** Dyadic product, see https://en.wikipedia.org/wiki/Dyadics#Dyadic.2C_outer.2C_and_tensor_products . 
    * 
    * Equivalent to the outer product when the scalars are reals (no complex conjugation is performed on
    * the inputs).
    */
  def dyad(lhs: Vec[A], rhs: Vec[A])(implicit A: MultiplicativeSemigroup[A]): MA = tabulate(lhs.length, rhs.length) { (r, c) => A.times(lhs(r), rhs(c)) }

  /* Alternative
  def kron(lhs: Mat[A], rhs: Mat[A]): MA =
    tabulate(lhs.nRows * rhs.nRows, lhs.nCols * rhs.nCols) { (r, c) =>
      val rr = r % rhs.nRows
      val rl = r / rhs.nRows
      val cr = c % rhs.nCols
      val cl = c / rhs.nCols
      lhs(rl, cl) * rhs(rr, cr)
    }
   */

  /** Kronecker product. */
  def kron(x: Mat[A], y: Mat[A])(implicit A: MultiplicativeSemigroup[A]): MA =
    blockTabulate(x.nRows, x.nCols)( (r, c) => x(r, c) *: y )

  //// Creation

  /** Matrix filled with zeroes. */
  def zeros(rows: Int, cols: Int)(implicit A: AdditiveMonoid[A]): MA = fillConstant(rows, cols)(A.zero)

  /** Identity matrix. */
  def eye(n: Int)(implicit A: Rig[A]): MA = tabulate(n, n)( (r, c) => if (r == c) A.one else A.zero )

  def toDiagMat(lhs: Vec[A])(implicit A: AdditiveMonoid[A]): MA = tabulate(lhs.length, lhs.length)( (r, c) => if (r == c) lhs(r) else A.zero )

  //// Additive group methods

  def plus(lhs: Mat[A], rhs: Mat[A])(implicit A: AdditiveSemigroup[A]): MA = pointwiseBinary(lhs, rhs)(_ + _)

  def negate(lhs: Mat[A])(implicit A: AdditiveGroup[A]): MA = pointwiseUnary(lhs)(-_)

  def minus(lhs: Mat[A], rhs: Mat[A])(implicit A: AdditiveGroup[A]): MA = pointwiseBinary(lhs, rhs)(_ - _)

  def pointwisePlus(lhs: Mat[A], rhs: A)(implicit A: AdditiveSemigroup[A]): MA = pointwiseUnary(lhs)(_ + rhs)

  def pointwiseMinus(lhs: Mat[A], rhs: A)(implicit A: AdditiveGroup[A]): MA = pointwiseUnary(lhs)(_ - rhs)

  //// Ring methods

  /** Matrix-matrix product. Requires `lhs.cols == rhs.rows`. */
  def times(lhs: Mat[A], rhs: Mat[A])(implicit A: Ring[A]): MA = {
    import spire.syntax.cfor._
    val n = lhs.nCols
    require(n == rhs.nRows)
    if (n == 0)
      zeros(lhs.nRows, rhs.nCols)
    else
      tabulate(lhs.nRows, rhs.nCols) { (r, c) =>
        var sum = lhs(r, 0) * rhs(0, c)
        cforRange(1 until lhs.nCols) { k =>
          sum += lhs(r, k) * rhs(k, c)
        }
        sum
      }
  }

  /** Frobenius product, sum of the Hadamard product elements.
    * 
    * See https://en.wikipedia.org/wiki/Frobenius_inner_product .*/
  def frobenius(lhs: Mat[A], rhs: Mat[A])(implicit A: Ring[A]): A = {
    val nr = lhs.nRows
    require(nr == rhs.nRows)
    val nc = lhs.nCols
    require(nc == rhs.nCols)
    import spire.syntax.cfor._
    var sum = A.zero
    cforRange(0 until nr) { r =>
      cforRange(0 until nc) { c =>
        sum += lhs(r, c) * rhs(r, c)
      }
    }
    sum
  }

  def pointwiseDiv(lhs: Mat[A], rhs: Mat[A])(implicit A: Field[A]): MA = pointwiseBinary(lhs, rhs)(A.div)

  def div(lhs: Mat[A], rhs: A)(implicit A: Field[A]): MA = pointwiseUnary(lhs)(A.div(_, rhs))

  //// Requires conjugation

  /** Returns the matrix conjugate. Does not transpose the matrix. */
  def conjugate(mat: Mat[A])(implicit A: Conjugation[A]): MA = map(mat)(A.conjugate)

  /** Returns the conjugate transpose, i.e. is equal to conjugate(t(mat)). */
  def ct(mat: Mat[A])(implicit A: Conjugation[A]): MA = tabulate(mat.nCols, mat.nRows)((i, j) => A.conjugate(mat(j, i)))
}
