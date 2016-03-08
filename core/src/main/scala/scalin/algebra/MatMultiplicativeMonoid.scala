package scalin
package algebra

import spire.algebra._

trait MatMultiplicativeMonoid[A, MA <: Mat[A]] extends MatFactory[A, MA] {

  implicit def scalar: MultiplicativeMonoid[A]

  //// Creation

  def ones(rows: Int, cols: Int): MA

  //// With `MultiplicativeMonoid[A]`, returning scalar

  def product(lhs: Mat[A]): A

  //// With `MultiplicativeMonoid[A]`, returning matrix

  /** Scalar-matrix product. */
  def times(lhs: A, rhs: Mat[A]): MA

  /** Matrix-scalar product. */
  def times(lhs: Mat[A], rhs: A): MA

  /** Pointwise multiplication, i.e. Hadamard product, see https://en.wikipedia.org/wiki/Hadamard_product_%28matrices%29 . */
  def pointwiseTimes(lhs: Mat[A], rhs: Mat[A]): MA

  /** Dyadic product, see https://en.wikipedia.org/wiki/Dyadics#Dyadic.2C_outer.2C_and_tensor_products . 
    * 
    * Equivalent to the outer product when the scalars are reals (no complex conjugation is performed on
    * the inputs).
    */
  def dyad(lhs: Vec[A], rhs: Vec[A]): MA

  /** Kronecker product. */
  def kron(lhs: Mat[A], rhs: Mat[A]): MA

}
