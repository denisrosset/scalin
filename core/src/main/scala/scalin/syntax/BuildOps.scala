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

final class TabulateOps[A](val dummy: Null) extends AnyVal {

  def apply[VA <: Vec[A]](length: Int)(f: Int => A)(implicit ev: VecEngine[A, VA]): VA = ev.tabulate(length)(f)

  def apply[MA <: Mat[A]](rows: Int, cols: Int)(f: (Int, Int) => A)(implicit ev: MatEngine[A, MA]): MA = ev.tabulate(rows, cols)(f)

}

final class MatOps[A](val dummy: Null) extends AnyVal {

  def apply[P <: Product, MA <: Mat[A]](first: P, next: P*)(implicit lr: MatOps.LiteralRow[P, A], ev: MatEngine[A, MA]) : MA = {
    ev.tabulate(next.length + 1, lr.length(first)) { (r, c) => if (r == 0) lr.get(first, c) else lr.get(next(r - 1), c) }
  }

}

object MatOps {

  trait LiteralRow[P <: Product, A] {

    def length(p: P): Int = p.productArity

    def get(p: P, k: Int): A = p.productElement(k).asInstanceOf[A]

  }

  object instance extends LiteralRow[Product, AnyRef]

  implicit def literal2[A]: LiteralRow[Tuple2[A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple2[A, A], A]]

  implicit def literal3[A]: LiteralRow[Tuple3[A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple3[A, A, A], A]]

  implicit def literal4[A]: LiteralRow[Tuple4[A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple4[A, A, A, A], A]]

  implicit def literal5[A]: LiteralRow[Tuple5[A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple5[A, A, A, A, A], A]]

  implicit def literal6[A]: LiteralRow[Tuple6[A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple6[A, A, A, A, A, A], A]]

  implicit def literal7[A]: LiteralRow[Tuple7[A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple7[A, A, A, A, A, A, A], A]]

  implicit def literal8[A]: LiteralRow[Tuple8[A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple8[A, A, A, A, A, A, A, A], A]]

  implicit def literal9[A]: LiteralRow[Tuple9[A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple9[A, A, A, A, A, A, A, A, A], A]]

  implicit def literal10[A]: LiteralRow[Tuple10[A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple10[A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal11[A]: LiteralRow[Tuple11[A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple11[A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal12[A]: LiteralRow[Tuple12[A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple12[A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal13[A]: LiteralRow[Tuple13[A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple13[A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal14[A]: LiteralRow[Tuple14[A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] =
    instance.asInstanceOf[LiteralRow[Tuple14[A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal15[A]: LiteralRow[Tuple15[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple15[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal16[A]: LiteralRow[Tuple16[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple16[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal17[A]: LiteralRow[Tuple17[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple17[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal18[A]: LiteralRow[Tuple18[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple18[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal19[A]: LiteralRow[Tuple19[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple19[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal20[A]: LiteralRow[Tuple20[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple20[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal21[A]: LiteralRow[Tuple21[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple21[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

  implicit def literal22[A]: LiteralRow[Tuple22[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A] = 
    instance.asInstanceOf[LiteralRow[Tuple22[A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A], A]]

}