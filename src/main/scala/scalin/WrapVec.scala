package scalin

trait WrapVec[-V[A] <: Vec[A]] {

  type RowVec[A] <: scalin.RowVec[A]

  def rowVec[A](v: V[A]): RowVec[A]

}

abstract class WrapVec0 {

  implicit object genericVec extends WrapVec[scalin.Vec] {

    type RowVec[A] = scalin.RowVec[A]

    def rowVec[A](v: scalin.Vec[A]): scalin.RowVec[A] = scalin.RowVec(v)

  }

}

object WrapVec extends WrapVec0 {

  implicit object mutableVec extends WrapVec[mutable.Vec] {

    type RowVec[A] = mutable.RowVec[A]

    def rowVec[A](v: mutable.Vec[A]): mutable.RowVec[A] = mutable.RowVec(v)

  }

  implicit object immutableVec extends WrapVec[immutable.Vec] {

    type RowVec[A] = immutable.RowVec[A]

    def rowVec[A](v: immutable.Vec[A]): immutable.RowVec[A] = immutable.RowVec(v)

  }

}
