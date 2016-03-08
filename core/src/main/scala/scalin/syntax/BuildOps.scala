package scalin
package syntax

import algebra._

final class ColMajorOps[A](val dummy: Null) extends AnyVal {

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(elements: A*)(implicit ev: MatEngine[A, MA]): MA =
    ev.colMajor(rows, cols)(elements: _*)

}

final class ColMatOps[A](val dummy: Null) extends AnyVal {

  def apply[MA <: Mat[A]](elements: A*)(implicit ev: MatEngine[A, MA]): MA = ev.colMat(elements: _*)

}

final class EyeOps[A](val dummy: Null) extends AnyVal {

  def apply[MA <: Mat[A]](n: Int)(implicit ev: MatRing[A, MA]): MA = ev.eye(n)

}

final class FillOps[A](val dummy: Null) extends AnyVal {

  def apply[VA <: Vec[A]](length: Int)(a: => A)(implicit ev: VecEngine[A, VA]): VA = ev.fill(length)(a)

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(a: => A)(implicit ev: MatEngine[A, MA]): MA = ev.fill(rows, cols)(a)

}

final class OnesOps[A](val dummy: Null) extends AnyVal {

  def apply[VA <: Vec[A]](length: Int)(implicit ev: VecRing[A, VA]): VA = ev.ones(length)

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, MA]): MA = ev.ones(rows, cols)

}

final class RowMajorOps[A](val dummy: Null) extends AnyVal {

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(elements: A*)(implicit ev: MatEngine[A, MA]): MA = ev.rowMajor(rows, cols)(elements: _*)

}

final class RowMatOps[A](val dummy: Null) extends AnyVal {

  def apply[MA <: Mat[A]](elements: A*)(implicit ev: MatEngine[A, MA]): MA = ev.rowMat(elements: _*)

}

final class VecOps[A](val dummy: Null) extends AnyVal {

  def apply[VA <: Vec[A]](elements: A*)(implicit ev: VecEngine[A, VA]): VA = ev.fromSeq(elements)

}

final class ZerosOps[A](val dummy: Null) extends AnyVal {

  def apply[VA <: Vec[A]](length: Int)(implicit ev: VecRing[A, VA]): VA = ev.zeros(length)

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, MA]): MA = ev.zeros(rows, cols)

}
