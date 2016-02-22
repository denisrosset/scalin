package scalin
package mutable

import spire.algebra._
import spire.syntax.field._
import spire.syntax.cfor._

import immutable.ConstantMat

trait AbstractMat[A] extends scalin.AbstractMat[A] { lhs =>

  def update(r: Int, c: Int, a: A): Unit

  protected def set(rhs: scalin.AbstractMat[A]): Unit = {
    require(rhs.rows == rows)
    require(rhs.cols == cols)
    cforRange(0 until rows) { r =>
      cforRange(0 until cols) { c =>
        update(r, c, rhs(r, c))
      }
    }
  }

  protected def copyNeeded(rhs: scalin.AbstractMat[A]): Boolean

  protected def copyIfNeeded(rhs: scalin.AbstractMat[A]): scalin.AbstractMat[A] =
    if (copyNeeded(rhs)) rhs.get else rhs

  def :=(rhs: scalin.AbstractMat[A]): Unit = set(copyIfNeeded(rhs))

}
