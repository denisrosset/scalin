package scalin

trait Wrap[-V[A] <: Vec[A]] {

  type R[A] <: RowVec[A]

  def wrap[A](v: V[A]): R[A]

}

abstract class Wrap0 {

  implicit object genericWrap extends Wrap[Vec] {
    type R[A] = RowVec[A]
    def wrap[A](v: Vec[A]): RowVec[A] = RowVec(v)
  }

//  implicit def getGeneric[V[A] <: Vec[A]]: Wrap[V] = genericInstance.asInstanceOf[Wrap[V]]

}

object Wrap extends Wrap0 {

  implicit object mutableWrap extends Wrap[mutable.Vec] {
    type R[A] = mutable.RowVec[A]
    def wrap[A](v: mutable.Vec[A]): mutable.RowVec[A] = mutable.RowVec(v)
  }

  implicit object immutableWrap extends Wrap[immutable.Vec] {
    type R[A] = immutable.RowVec[A]
    def wrap[A](v: immutable.Vec[A]): immutable.RowVec[A] = immutable.RowVec(v)
  }

}
