package scalin

package object mutable {

  type MatEngine[A] = scalin.MatEngine[A, mutable.Mat[A]]

  type VecEngine[A] = scalin.VecEngine[A, mutable.Vec[A]]

  def newLength(length: Int): Int =
    if (length == 0) { 4 } else if (length < 0x0400) { length * 2 } else if (length < 0x0800) {
      length + 0x0400
    } else if (length < 0x1000) { length + 0x0800 } else if (length < 0x2000) {
      length + 0x1000
    } else if (length < 0x4000) { length + 0x2000 } else { length + 0x4000 }
}
