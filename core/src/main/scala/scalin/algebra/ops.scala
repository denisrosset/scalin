package scalin
package algebra

import spire.algebra._

abstract class ops0[V[A] <: Vec[A], M[A] <: Mat[A]] {

  // methods to implement

  def defaultTabulate[A](length: Int)(f: Int => A): V[A]

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): M[A]

  implicit def vecFactory[A]: VecFactory[A, V[A]] = new VecFactory[A, V[A]] {
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matFactory[A]: MatFactory[A, M[A]] = new MatFactory[A, M[A]] {
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}

abstract class ops1[V[A] <: Vec[A], M[A] <: Mat[A]] extends ops0[V, M] {

  implicit def vecRing[A](implicit A: Ring[A]): VecRing[A, V[A]] = new VecRing[A, V[A]] {
    def scalar = A
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matRing[A](implicit A: Ring[A]): MatRing[A, M[A]] = new MatRing[A, M[A]] {
    def scalar = A
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}

abstract class ops2[V[A] <: Vec[A], M[A] <: Mat[A]] extends ops1[V, M] {

  implicit def vecEuclideanRing[A](implicit A: EuclideanRing[A]): VecEuclideanRing[A, V[A]] = new VecEuclideanRing[A, V[A]] {
    def scalar = A
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matEuclideanRing[A](implicit A: EuclideanRing[A]): MatEuclideanRing[A, M[A]] = new MatEuclideanRing[A, M[A]] {
    def scalar = A
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}

abstract class ops[V[A] <: Vec[A], M[A] <: Mat[A]] extends ops2[V, M] {

  implicit def vecField[A](implicit A: Field[A]): VecField[A, V[A]] = new VecField[A, V[A]] {
    def scalar = A
    def tabulate(length: Int)(f: Int => A): V[A] = defaultTabulate[A](length)(f)
  }

  implicit def matField[A](implicit A: Field[A]): MatField[A, M[A]] = new MatField[A, M[A]] {
    def scalar = A
    def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A): M[A] = defaultTabulate[A](rows, cols)(f)
  }

}
