package scalin

import spire.syntax.cfor._

trait Slice {

  def length: Int

  def apply(k: Int): Int

}

object Slice {

  class ArraySlice(array: Array[Int]) extends Slice {

    def length = array.length

    def apply(k: Int): Int = array(k)

  }

  class SeqSlice(seq: Seq[Int]) extends Slice {

    def length = seq.size

    def apply(k: Int): Int = seq(k)

  }

  implicit def fromSeq(seq: Seq[Int]): Slice = new SeqSlice(seq)

  implicit def fromMask(mask: Vec[Boolean]): Slice = {
    val array = new Array[Int](mask.count)
    var ak = 0
    cforRange(0 until mask.length) { mk =>
      if (mask(mk)) {
        array(ak) = mk
        ak += 1
      }
    }
    new ArraySlice(array)
  }

}
