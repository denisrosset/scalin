package scalin
package mutable

import spire.algebra._

trait Vec[A] extends scalin.Vec[A] with mutable.AbstractVec[A]

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    DenseVec.tabulate[A](length)(f)

}
