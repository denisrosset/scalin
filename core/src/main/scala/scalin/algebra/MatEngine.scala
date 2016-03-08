package scalin
package algebra

import spire.algebra._

/** Builder for matrices with an arbitrary scalar type `A`. */
trait MatEngine[A, MA <: Mat[A]] {

  type Ret = MA // hack for the return type of Mat.flatten

  //// Creation

  // empty matrix is an ill-defined object (0x0, nx0 and 0xn are all empty)

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): MA

  def fill(rows: Int, cols: Int)(a: => A): MA

  def colMajor(rows: Int, cols: Int)(elements: A*): MA

  def rowMajor(rows: Int, cols: Int)(elements: A*): MA

  def rowMat(elements: A*): MA

  def colMat(elements: A*): MA

  def toRowMat(lhs: Vec[A]): MA

  def toColMat(lhs: Vec[A]): MA

  //// Standard Java methods

  def equal(lhs: Mat[A], rhs: Mat[A]): Boolean

  /** Hashcode compatible with the reference algorithm provided in scalin.impl.VecEngine. */
  def hashCode(lhs: Mat[A]): Int

  //// Collection-like methods

  /** Returns the number of elements satisfying the predicate `f`. */
  def count(lhs: Mat[A])(f: A => Boolean): Int

  /** Folds the elements of the matrix using the specified associative binary operator.
    * 
    * The order in which operations are performed on elements is unspecified and 
    * may be nondeterministic. 
    */
  def fold[A1 >: A](lhs: Mat[A])(z: A1)(op: (A1, A1) => A1): A1

  /** Builds a new matrix by applying a function to all elements of this matrix. */
  def map[B](lhs: Mat[B])(f: B => A): MA = tabulate(lhs.rows, lhs.cols)( (r, c) => f(lhs(r, c)) )

  /** Returns the horizontal concatenation of two matrices with the same number of rows. */
  def horzcat(lhs: Mat[A], rhs: Mat[A]): MA

  /** Returns the vertical concatenation of two matrices with the same number of columns. */
  def vertcat(lhs: Mat[A], rhs: Mat[A]): MA

  /** Flatten a block matrix. Not defined if the matrix is empty. */
  def flatten[B <: Mat[A]](lhs: Mat[B]): MA

  /** Returns the flattened block matrix specified by `lhs.map(f)`. Not defined if the matrix is empty. */
  def flatMap[B](lhs: Mat[B])(f: B => Mat[A]): MA

  //// Shuffling elements around

  /** Returns the matrix transpose. Does not conjuagates complex numbers. */
  def t(mat: Mat[A]): MA

  /** Reshapes a vector in a matrix shape, using column-major ordering of elements. */ 
  def reshape(vec: Vec[A], rows1: Int, cols1: Int): MA

  /** Returns a matrix slice of a matrix. 
    * The return value is a copy (i.e. not read- or write-through as in scala.breeze). */
  def slice(mat: Mat[A], rs: Subscript, cs: Subscript): MA

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA

  def pointwiseEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: B)(implicit ev: Boolean =:= A): MA

  def pointwiseNotEqual[B](lhs: Mat[B], rhs: Mat[B])(implicit ev: Boolean =:= A): MA

  def pointwiseEqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA

  def pointwiseEqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA

  def pointwiseNeqv[B](lhs: Mat[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): MA

  def pointwiseNeqv[B](lhs: Mat[B], rhs: Mat[B])(implicit B: Eq[B], ev: Boolean =:= A): MA

  //// With `Eq[A]`

  def eqv(lhs: Mat[A], rhs: Mat[A])(implicit eqv: Eq[A]): Boolean

}
