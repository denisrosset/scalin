package scalin
package algebra

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.field._

import scalin.syntax.assign._

/** Builder for matrices with an arbitrary scalar type `A`. */
trait MatEngine[A, +MA <: Mat[A]] { self =>

  implicit def MA: MatEngine[A, MA] = self

  //// Minimal methods to implement

  /** Creates a vector from the given size (nRows, nCols) and a value function. */
  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): MA

  /** Builds a mtrix from the given size and a user-provided function that mutates
    * a temporary mutable matrix.
    */
  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit): MA

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

  type Ret <: MA // hack for the return type of Mat.flatten

  //// Creation

  // empty matrix is an ill-defined object (0x0, nx0 and 0xn are all empty)

  def fill(nRows: Int, nCols: Int)(a: => A): MA = tabulate(nRows, nCols)( (i, j) => a )

  /* Alternative
  def fillConstant(rows: Int, cols: Int)(a: A): MA = fill(rows, cols)(a)
   */
  def fillConstant(nRows: Int, nCols: Int)(a: A): MA = fromMutable(nRows, nCols) { res =>
    cforRange(0 until nCols) { c =>
      cforRange(0 until nRows) { r =>
        res(r, c) := a
      }
    }
  }

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

  /** Returns the number of elements satisfying the predicate `f`. */
  def count(lhs: Mat[A])(f: A => Boolean): Int = {
    var n = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        if (f(lhs(r, c)))
          n += 1
      }
    }
    n
  }

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
    else {
      val els = new Array[Mat[A]](lhs.nRows * lhs.nCols)
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          els(r + c * lhs.nRows) = f(lhs(r, c))
        }
      }
      flattenArray(els, lhs.nRows, lhs.nCols)
    }

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
    else {
      val els = new Array[Mat[A]](lhs.nRows * lhs.nCols)
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
          els(r + c * lhs.nRows) = lhs(r, c)
        }
      }
      flattenArray(els, lhs.nRows, lhs.nCols)
    }

  /** Folds the elements of the matrix using the specified associative binary operator.
    * 
    * The order in which operations are performed on elements is unspecified and 
    * may be nondeterministic. 
    */
  def fold[A1 >: A](lhs: Mat[A])(z: A1)(op: (A1, A1) => A1): A1 =
    if (lhs.nRows == 0 || lhs.nCols == 0) z
    else if (lhs.nRows == 1 && lhs.nCols == 1) lhs(0, 0)
    else {
      var acc = z // could be optimized
      var i = 0
      // in column-major order
      cforRange(0 until lhs.nCols) { c =>
        cforRange(0 until lhs.nRows) { r =>
          acc = op(acc, lhs(r, c))
        }
      }
      acc
    }

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
  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val m = lhs.nRows
    require(m == rhs.nRows)
    val nl = lhs.nCols
    val nr = rhs.nCols
    fromMutable(m, nl + nr) { res =>
      res(::, 0 until nl) := lhs
      res(::, nl until nl + nr) := rhs
    }
  }

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
  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA = {
    val n = lhs.nCols
    require(n == rhs.nCols)
    val ml = lhs.nRows
    val mr = rhs.nRows
    fromMutable(ml + mr, n) { res =>
      res(0 until ml, ::) := lhs
      res(ml until ml + mr, ::) := rhs
    }
  }

  protected def flattenArray(array: Array[Mat[A]], blockRows: Int, blockCols: Int): MA = {
    def block(br: Int, bc: Int): Mat[A] = array(br + bc * blockRows)
    require(blockRows > 0 && blockCols > 0)
    var rows = 0
    var cols = 0
    cforRange(0 until blockCols) { bc =>
      cols += block(0, bc).nCols
    }
    cforRange(0 until blockRows) { br =>
      rows += block(br, 0).nRows
    }
    fromMutable(rows, cols) { res =>
      var row = 0
      cforRange(0 until blockRows) { br =>
        var col = 0
        val nr = block(br, 0).nRows
        cforRange(0 until blockCols) { bc =>
          val b = block(br, bc)
          require(b.nRows == nr)
          res(row until row + b.nRows, col until col + b.nCols) := b
          col += b.nCols
        }
        row += nr
      }
    }
  }

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

}
