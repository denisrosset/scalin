package scalin
package algebra

import spire.algebra._

abstract class ops0[V[A] <: Vec[A], M[A] <: Mat[A]] {

  // methods to implement

  def defaultTabulate[A](length: Int)(f: Int => A): V[A]

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): M[A]

  // default implementations of operations

  implicit def vecTrait[A]: VecTrait[A, V] = new VecTrait[A, V] {
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matTrait[A]: MatTrait[A, M] = new MatTrait[A, M] {
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}

abstract class ops[V[A] <: Vec[A], M[A] <: Mat[A]] extends ops0[V, M] {

  // default implementations of operations

  implicit def vecRing[A](implicit A: Ring[A]): VecRing[A, V] = new VecRing[A, V] {
    def scalar = A
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matRing[A](implicit A: Ring[A]): MatRing[A, M] = new MatRing[A, M] {
    def scalar = A
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}
