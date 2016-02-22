package scalin

import spire.algebra._

/** A trait representing a concrete row vector, or a row vector node in 
  * an abstract syntax tree. */
abstract class AbstractRowVec[A] extends AbstractNode {

  def apply(k: Int): A

  def length: Int

  def t: AbstractVec[A]

  def value[V[A] <: Vec[A], R[A] <: RowVec[A, V[A]]](implicit f: AbstractVec[A] => V[A], w: RowVecWrap[V, R]): R[A] = w.wrap(f(t))


}

