package scalin

trait WrapVec[-V[A] <: Vec[A]] {

  type RowVec[A] <: scalin.RowVec[A]
  type SliceVec[A] <: scalin.SliceVec[A]

  def rowVec[A](v: V[A]): RowVec[A]
  def sliceVec[A](v: V[A], slice: Seq[Int]): SliceVec[A]

}

abstract class WrapVec0 {

  implicit object genericVec extends WrapVec[scalin.Vec] {

    type RowVec[A] = scalin.RowVec[A]
    type SliceVec[A] = scalin.SliceVec[A]

    def rowVec[A](v: scalin.Vec[A]): scalin.RowVec[A] = scalin.RowVec(v)
    def sliceVec[A](v: scalin.Vec[A], slice: Seq[Int]): scalin.SliceVec[A] = scalin.SliceVec[A](v, slice)

  }

}

object WrapVec extends WrapVec0 {

  implicit object mutableVec extends WrapVec[mutable.Vec] {

    type RowVec[A] = mutable.RowVec[A]
    type SliceVec[A] = mutable.SliceVec[A]

    def rowVec[A](v: mutable.Vec[A]): mutable.RowVec[A] = mutable.RowVec(v)
    def sliceVec[A](v: mutable.Vec[A], slice: Seq[Int]): mutable.SliceVec[A] = mutable.SliceVec[A](v, slice)

  }

  implicit object immutableVec extends WrapVec[immutable.Vec] {

    type RowVec[A] = immutable.RowVec[A]
    type SliceVec[A] = immutable.SliceVec[A]

    def rowVec[A](v: immutable.Vec[A]): immutable.RowVec[A] = immutable.RowVec(v)
    def sliceVec[A](v: immutable.Vec[A], slice: Seq[Int]): immutable.SliceVec[A] = immutable.SliceVec[A](v, slice)

  }

}
