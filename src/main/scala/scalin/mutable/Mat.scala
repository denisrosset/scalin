package scalin
package mutable

import spire.algebra._
import spire.syntax.cfor._

trait Mat[A] extends scalin.Mat[A] {

  // 1x1

  def set(r: Int, c: Int, a: A): Unit

  // 1xn and nx1

  def set(r: Int, cs: Subscript, rhs: A): Unit = {
    val ci = cs.forLength(cols)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhs)
    }
  }

  def set(r: Int, cs: Subscript, rhs: Vec[A]): Unit = {
    val ci = cs.forLength(cols)
    val n = ci.length
    require(n == rhs.length)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhs(ck))
    }
  }

  def set(rs: Subscript, c: Int, rhs: A): Unit = {
    val ri = rs.forLength(rows)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhs)
    }
  }

  def set(rs: Subscript, c: Int, rhs: Vec[A]): Unit = {
    val ri = rs.forLength(rows)
    val n = ri.length
    require(n == rhs.length)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhs(rk))
    }
  }

  // nxn

  def set(rs: Subscript, cs: Subscript, rhs: A): Unit = {
    val ri = rs.forLength(rows)
    val ci = cs.forLength(cols)
    cforRange(0 until ri.length) { rk =>
      cforRange(0 until ci.length) { ck =>
        set(ri(rk), ci(ck), rhs)
      }
    }
  }

  def set(rs: Subscript, cs: Subscript, rhs: Mat[A]): Unit = {
    val ri = rs.forLength(rows)
    val ci = cs.forLength(cols)
    require(ri.length == rhs.rows)
    require(ci.length == rhs.cols)
    cforRange(0 until ri.length) { rk =>
      cforRange(0 until ci.length) { ck =>
        set(ri(rk), ci(ck), rhs(rk, ck))
      }
    }
  }

  // flattening

  def set(sub: Subscript, rhs: A): Unit = {
    val ind = sub.forLength(rows * cols)
    cforRange(0 until ind.length) { k =>
      val ik = ind(k)
      val r = ik % rows
      val c = ik / rows
      set(r, c, rhs)
    }
  }

  def set(sub: Subscript, rhs: Vec[A]): Unit = {
    val ind = sub.forLength(rows * cols)
    require(rhs.length == ind.length)
    cforRange(0 until ind.length) { k =>
      val ik = ind(k)
      val r = ik % rows
      val c = ik / rows
      set(r, c, rhs(k))
    }
  }

}

object Mat extends MatFactory[Mat, Dummy] {

  def tabulate[A:Dummy](rows: Int, cols: Int)( f: (Int, Int) => A ): Mat[A] =
    DenseMat.tabulate[A](rows, cols)(f)

}
