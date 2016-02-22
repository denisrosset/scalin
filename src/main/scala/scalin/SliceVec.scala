package scalin

import spire.syntax.cfor._

trait SliceVec[A] extends AbstractVec[A] {

  def vec: Vec[A]

  def slice: Seq[Int]

  def length = slice.size

  def apply(k: Int) = vec(slice(k))

  def intersectsMutable(rhs: mutable.Vec[_], ks: Opt[Range]) =
    if (vec.intersects
    ks match {
    case Opt
  // TODO: be clever
  def nextNonZero(k: Int) = k + 1

}

object SliceVec {

  def apply[A](vec0: Vec[A], slice0: Seq[Int]): SliceVec[A] = new SliceVec[A] {

    def vec = vec0

    def slice = slice0

  }

}
