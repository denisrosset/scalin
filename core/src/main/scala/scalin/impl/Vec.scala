package scalin
package impl

import spire.syntax.cfor._

object Vec {

  def equal(lhs: Vec[_], rhs: Vec[_]): Boolean = (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (lhs(k) != rhs(k)) return false
      }
      true
  }

  def hashCode(lhs: Vec[_]): Int = {
    import scala.util.hashing.MurmurHash3._
    val seed = 0x3CA7195E
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until lhs.length) { k =>
      val hv = lhs(k).##
      if (hv != 0) {
        val hkv = k * 41 + hv
        a += hkv
        b *= (hkv | 1)
        n += 1
      }
    }
    var h = seed
    h = mix(h, lhs.length)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  def countTrue[A](lhs: Vec[A])(implicit ev: A =:= Boolean): Int = {
    import spire.syntax.cfor._
    var sum = 0
    cforRange(0 until lhs.length) { k => if (lhs(k): Boolean) sum += 1 }
    sum
  }

}
