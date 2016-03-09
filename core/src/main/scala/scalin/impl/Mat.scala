package scalin
package impl

import spire.syntax.cfor._

object Mat {

  def equal(lhs: Mat[_], rhs: Mat[_]): Boolean =
    (lhs.rows == rhs.rows && lhs.cols == rhs.cols) && {
      cforRange(0 until lhs.rows) { r =>
        cforRange(0 until lhs.cols) { c =>
          if (lhs(r, c) != rhs(r, c)) return false
        }
      }
      true
    }

  def hashCode(lhs: Mat[_]): Int = {
    import scala.util.hashing.MurmurHash3._
    val seed = 0x3CA7198A
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until lhs.rows) { r =>
      cforRange(0 until lhs.cols) { c =>
        val hv = lhs(r, c).##
        if (hv != 0) {
          val hkv = (r * 41 + c) * 41 + hv
          a += hkv
          b *= (hkv | 1)
          n += 1
        }
      }
    }
    var h = seed
    h = mix(h, lhs.rows)
    h = mix(h, lhs.cols)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  def countTrue[A](lhs: Mat[A])(implicit ev: A =:= Boolean): Int = {
    var sum = 0
    cforRange(0 until lhs.rows) { r =>
      cforRange(0 until lhs.cols) { c =>
        if (lhs(r, c): Boolean)
          sum += 1
      }
    }
    sum
  }

}
