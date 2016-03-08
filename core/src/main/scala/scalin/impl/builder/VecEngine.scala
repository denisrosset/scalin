package scalin
package impl
package builder

import spire.syntax.cfor._

import scalin.syntax.assign._

trait VecEngine[A, VA <: Vec[A], UA <: mutable.Vec[A]] extends scalin.impl.VecEngine[A, VA] {

  //// Mutable variant

  implicit def UA: scalin.algebra.VecEngine[A, UA]

  /** Returns a mutable vector of the given length. The initial content of the vector is undefined. */
  def alloc(length: Int): UA

  /** Converts the mutable vector `mutable` into the desired instance. `mutable` is destroyed in
    * the process. */
  def result(mutable: UA): VA

  //// Creation

  def fillConstant(length: Int)(a: A): VA = {
    val res = alloc(length)
    cforRange(0 until length) { k => res(k) := a }
    result(res)
  }

  //// Collection-like methods

  def cat(lhs: Vec[A], rhs: Vec[A]): VA = {
    val nl = lhs.length
    val nr = rhs.length
    val res = alloc(nl + nr)
    cforRange(0 until nl) { k => res(k) := lhs(k) }
    cforRange(0 until nr) { k => res(nl + k) := rhs(k) }
    result(res)
  }

  protected def catArray(array: Array[Vec[A]]): VA = {
    val n = array.length
    var len = 0
    cforRange(0 until n) { j =>
      len += array(j).length
    }
    val res = alloc(len)
    var i = 0
    cforRange(0 until n) { j =>
      val lenj = array(j).length
      res(i until (i + lenj)) := array(j)
      i += lenj
    }
    result(res)
  }

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) fromVec(f(lhs(0)))
    else {
      val els = new Array[Vec[A]](lhs.length)
      cforRange(0 until lhs.length) { j =>
        els(j) = f(lhs(j))
      }
      catArray(els)
    }

  def flatten[B <: Vec[A]](lhs: Vec[B]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) fromVec(lhs(0))
    else {
      val els = new Array[Vec[A]](lhs.length)
      cforRange(0 until lhs.length) { j =>
        els(j) = lhs(j)
      }
      catArray(els)
    }

}
