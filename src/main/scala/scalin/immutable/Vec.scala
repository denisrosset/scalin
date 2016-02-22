package scalin
package immutable

trait Vec[A] extends scalin.Vec[A] with Immutable

object Vec extends VecFactory[immutable.Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

}
