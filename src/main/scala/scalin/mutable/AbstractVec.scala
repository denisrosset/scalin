package scalin
package mutable

import spire.algebra._
import spire.syntax.cfor._

trait AbstractVec[A] extends scalin.AbstractVec[A] with Mutable { lhs =>

  def update(k: Int, a: A): Unit

  def :=(rhs: scalin.AbstractVec[A]): Unit = rhs.touch(AbstractVec.this) match {
    case Touch.Clean() | Touch.AsIs() =>
      cforRange(0 until length) { k =>
        update(k, rhs(k))
      }
    case _ =>
      AbstractVec.this := rhs.get
  }

  def :=(rhs: A): Unit = {
    cforRange(0 until length) { k =>
      update(k, rhs)
    }
  }

}
