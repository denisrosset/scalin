package scalin
package syntax

import algebra._

final class ColMajorOps[A](val dummy: Null) extends AnyVal {

  def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(elements: A*)(implicit ev: MatTrait[A, M]): M[A] =
    ev.colMajor(rows, cols)(elements: _*)

}

final class ColMatOps[A](val dummy: Null) extends AnyVal {

  def apply[M[A] <: Mat[A]](elements: A*)(implicit ev: MatTrait[A, M]): M[A] = ev.colMat(elements: _*)

}

final class EyeOps[A](val dummy: Null) extends AnyVal {

  def apply[M[A] <: Mat[A]](n: Int)(implicit ev: MatRing[A, M]): M[A] = ev.eye(n)

}

final class FillOps[A](val dummy: Null) extends AnyVal {

  def apply[A, V[A] <: Vec[A]](length: Int)(a: => A)(implicit ev: VecTrait[A, V]): V[A] = ev.fill(length)(a)

  def apply[A, M[A] <: Mat[A]](rows: Int, cols: Int)(a: => A)(implicit ev: MatTrait[A, M]): M[A] = ev.fill(rows, cols)(a)

}

final class OnesOps[A](val dummy: Null) extends AnyVal {

  def apply[V[A] <: Vec[A]](length: Int)(implicit ev: VecRing[A, V]): V[A] = ev.ones(length)

  def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, M]): M[A] = ev.ones(rows, cols)

}

final class RowMajorOps[A](val dummy: Null) extends AnyVal {

  def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(elements: A*)(implicit ev: MatTrait[A, M]): M[A] = ev.rowMajor(rows, cols)(elements: _*)

}

final class RowMatOps[A](val dummy: Null) extends AnyVal {

  def apply[M[A] <: Mat[A]](elements: A*)(implicit ev: MatTrait[A, M]): M[A] = ev.rowMat(elements: _*)

}

final class VecOps[A](val dummy: Null) extends AnyVal {

  def apply[V[A] <: Vec[A]](elements: A*)(implicit ev: VecTrait[A, V]): V[A] = ev.fromSeq(elements)

}

final class ZerosOps[A](val dummy: Null) extends AnyVal {

  def apply[V[A] <: Vec[A]](length: Int)(implicit ev: VecRing[A, V]): V[A] = ev.zeros(length)

  def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, M]): M[A] = ev.zeros(rows, cols)

}
