package scalin
package mutable

import spire.syntax.cfor._

trait Mat[A] extends scalin.Mat[A] {

  // 1x1

  def set(r: Int, c: Int, a: A): Unit

  // 1xn and nx1

  def set(r: Int, cs: Subscript, rhs: A): Unit = {
    val ci = cs.forLength(nCols)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhs)
    }
  }

  def set(r: Int, cs: Subscript, rhs: scalin.Vec[A]): Unit = {
    val ci = cs.forLength(nCols)
    val n = ci.length
    require(n == rhs.length)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhs(ck))
    }
  }

  def set(rs: Subscript, c: Int, rhs: A): Unit = {
    val ri = rs.forLength(nRows)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhs)
    }
  }

  def set(rs: Subscript, c: Int, rhs: scalin.Vec[A]): Unit = {
    val ri = rs.forLength(nRows)
    val n = ri.length
    require(n == rhs.length)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhs(rk))
    }
  }

  // nxn

  def set(rs: Subscript, cs: Subscript, rhs: A): Unit = {
    val ri = rs.forLength(nRows)
    val ci = cs.forLength(nCols)
    cforRange(0 until ri.length) { rk =>
      cforRange(0 until ci.length) { ck =>
        set(ri(rk), ci(ck), rhs)
      }
    }
  }

  def set(rs: Subscript, cs: Subscript, rhs: scalin.Mat[A]): Unit = {
    val ri = rs.forLength(nRows)
    val ci = cs.forLength(nCols)
    require(ri.length == rhs.nRows)
    require(ci.length == rhs.nCols)
    cforRange(0 until ri.length) { rk =>
      cforRange(0 until ci.length) { ck =>
        set(ri(rk), ci(ck), rhs(rk, ck))
      }
    }
  }

  // flattening

  def set(sub: Subscript, rhs: A): Unit = {
    val ind = sub.forLength(nRows * nCols)
    cforRange(0 until ind.length) { k =>
      val ik = ind(k)
      val r = ik % nRows
      val c = ik / nRows
      set(r, c, rhs)
    }
  }

  def set(sub: Subscript, rhs: scalin.Vec[A]): Unit = {
    val ind = sub.forLength(nRows * nCols)
    require(rhs.length == ind.length)
    cforRange(0 until ind.length) { k =>
      val ik = ind(k)
      val r = ik % nRows
      val c = ik / nRows
      set(r, c, rhs(k))
    }
  }

}
