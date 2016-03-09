package scalin
package algebra

import spire.algebra._

trait VecRing[A, VA <: Vec[A]] extends VecMultiplicativeMonoid[A, VA] {

  implicit def scalar: Ring[A]

  //// Creation

  def zeros(length: Int): VA

  //// With `A:AdditiveGroup`
  
  def plus(lhs: Vec[A], rhs: Vec[A]): VA

  def negate(lhs: Vec[A]): VA

  def minus(lhs: Vec[A], rhs: Vec[A]): VA

  def pointwisePlus(lhs: Vec[A], rhs: A): VA

  def pointwiseMinus(lhs: Vec[A], rhs: A): VA

  /** Number of contained non-zero elements. */
  def nnz(lhs: Vec[A])(implicit ev: Eq[A]): Int

  /** Sum of all the elements in the vector. */
  def sum(lhs: Vec[A]): A

  //// With `A:Ring`

  /** Vector-matrix product. The vector is interpreted as a row vector. */
  def times(lhs: Vec[A], rhs: Mat[A]): VA

  /** Matrix-vector product. The vector is interpreted as a column vector. */
  def times(lhs: Mat[A], rhs: Vec[A]): VA

  /** Dot product. Equivalent to the real inner product, but not the complex inner product.*/
  def dot(lhs: Vec[A], rhs: Vec[A]): A

}
