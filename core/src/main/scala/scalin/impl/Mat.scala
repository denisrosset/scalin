package scalin
package impl

import spire.syntax.cfor._

object Mat {

  def equal(lhs: Mat[_], rhs: Mat[_]): Boolean =
    (lhs.nRows == rhs.nRows && lhs.nCols == rhs.nCols) && {
      cforRange(0 until lhs.nRows) { r =>
        cforRange(0 until lhs.nCols) { c =>
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
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
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
    h = mix(h, lhs.nRows)
    h = mix(h, lhs.nCols)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  def countTrue[A](lhs: Mat[A])(implicit ev: A =:= Boolean): Int = {
    var sum = 0
    cforRange(0 until lhs.nRows) { r =>
      cforRange(0 until lhs.nCols) { c =>
        if (lhs(r, c): Boolean)
          sum += 1
      }
    }
    sum
  }

}
