package scalin
package mutable

import spire.algebra.AdditiveSemigroup
import spire.syntax.cfor._

trait Mat[A] extends scalin.Mat[A] { self =>

  def sharedData: Boolean

  def prepareMutation(): Unit

  // 1x1

  def set(r: Int, c: Int, a: A): Unit

  // useful for COOMat and sparse matrices that can have duplicate entries
  def add(r: Int, c: Int, a: A)(implicit ev: AdditiveSemigroup[A]): Unit = set(r, c, ev.plus(apply(r, c), a))

  // 1xn and nx1

  def set(r: Int, cs: Subscript, rhs: A): Unit = {
    val ci = cs.forLength(nCols)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhs)
    }
  }

  def set(r: Int, cs: Subscript, rhs: scalin.Vec[A]): Unit = {
    val rhsCopy = rhs.copyIfOverlap(self)
    val ci = cs.forLength(nCols)
    val n = ci.length
    require(n == rhsCopy.length)
    cforRange(0 until ci.length) { ck =>
      set(r, ci(ck), rhsCopy(ck))
    }
  }

  def set(rs: Subscript, c: Int, rhs: A): Unit = {
    val ri = rs.forLength(nRows)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhs)
    }
  }

  def set(rs: Subscript, c: Int, rhs: scalin.Vec[A]): Unit = {
    val rhsCopy = rhs.copyIfOverlap(self)
    val ri = rs.forLength(nRows)
    val n = ri.length
    require(n == rhsCopy.length)
    cforRange(0 until ri.length) { rk =>
      set(ri(rk), c, rhsCopy(rk))
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
    val rhsCopy = rhs.copyIfOverlap(self)
    val ri = rs.forLength(nRows)
    val ci = cs.forLength(nCols)
    require(ri.length == rhsCopy.nRows)
    require(ci.length == rhsCopy.nCols)
    cforRange(0 until ri.length) { rk =>
      cforRange(0 until ci.length) { ck =>
        set(ri(rk), ci(ck), rhsCopy(rk, ck))
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
    val rhsCopy = rhs.copyIfOverlap(self)
    val ind = sub.forLength(nRows * nCols)
    require(rhsCopy.length == ind.length)
    cforRange(0 until ind.length) { k =>
      val ik = ind(k)
      val r = ik % nRows
      val c = ik / nRows
      set(r, c, rhsCopy(k))
    }
  }

}

object Mat extends MatType[scalin.mutable.Mat] {

  type TC[A] = Dummy[A]

  def defaultEngine[A: TC]: MatEngine[A] = DenseMat.defaultEngine[A]

}
