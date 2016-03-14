package scalin

import spire.syntax.cfor._

sealed trait Subscript {

  def forLength(n: Int): Indices

}


object Subscript {

  object All extends Subscript {

    override def toString = "Subscript.All"
    def forLength(n: Int) = Indices.all(n)

  }

  implicit def all(arg: ::.type): Subscript = All

  implicit def vecWrap(vec: Vec[Int]): Subscript = new Indices.VecWrap(vec)

  implicit def seqWrap(seq: Seq[Int]): Subscript = new Indices.SeqWrap(seq)

  implicit def arrayWrap(array: Array[Int]): Subscript = new Indices.ArrayWrap(array)

  implicit def fromMask(mask: Vec[Boolean]): Subscript = {
    val array = new Array[Int](scalin.impl.Vec.countTrue(mask))
    var ak = 0
    cforRange(0 until mask.length) { mk =>
      if (mask(mk)) {
        array(ak) = mk
        ak += 1
      }
    }
    new Indices.ArrayWrap(array)
  }

  implicit def fromMask(mask: Mat[Boolean]): Subscript = {
    val array = new Array[Int](scalin.impl.Mat.countTrue(mask))
    var ak = 0
    cforRange(0 until mask.nRows) { rk =>
      cforRange(0 until mask.nCols) { ck =>
        if (mask(rk, ck)) {
          array(ak) = rk + ck * mask.nRows
          ak += 1
        }
      }
    }
    new Indices.ArrayWrap(array)
  }

}

sealed trait Indices extends Subscript { self =>

  def forLength(n: Int) = self

  def length: Int

  def apply(k: Int): Int

}

object Indices {

  def apply(indices: Int*): Indices = new SeqWrap(indices)

  def all(n: Int) = new All(n)

  final class All(val length: Int) extends Indices {

    override def toString = s"Indices.All($length)"
    def apply(k: Int) = k

  }

  final class ArrayWrap(array: Array[Int]) extends Indices {

    override def toString = array.mkString("Indices(", ", ", ")")
    def length = array.length
    def apply(k: Int) = array(k)

  }

  final class SeqWrap(seq: Seq[Int]) extends Indices {

    override def toString = seq.mkString("Indices(", ", ", ")")
    def length = seq.size
    def apply(k: Int) = seq(k)

  }

  final class VecWrap(vec: Vec[Int]) extends Indices {

    override def toString = (0 until vec.length).map(vec(_)).mkString("Indices(", ", ", ")")
    def length = vec.length
    def apply(k: Int) = vec(k)

  }

}
