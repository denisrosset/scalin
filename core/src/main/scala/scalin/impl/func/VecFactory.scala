package scalin
package impl
package func

import spire.syntax.cfor._

trait VecFactory[A, VA <: Vec[A]] extends scalin.impl.VecFactory[A, VA] {

  //// Creation

  def fillConstant(length: Int)(a: A): VA = fill(length)(a)

  //// Collection-like methods

  // They have slow implementations due to the absence of a mutable builder.
  
  def cat(lhs: Vec[A], rhs: Vec[A]): VA = {
    val nl = lhs.length
    val nr = rhs.length
    tabulate(nl + nr)( k => if (k < nl) lhs(k) else rhs(k - nl) )
  }

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](f(lhs(0)))(identity)
    else {
      var acc: VA = cat(f(lhs(0)), f(lhs(1)))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, f(lhs(k)))
      }
      acc
    }

  def flatten[B <: Vec[A]](lhs: Vec[B]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](lhs(0))(identity)
    else {
      var acc = cat(lhs(0), lhs(1))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, lhs(k))
      }
      acc
    }

}
