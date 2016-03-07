package scalin
package immutable

object ops extends scalin.algebra.ops[immutable.Vec, immutable.Mat] {

  import algebra._
  import spire.algebra._

  def defaultTabulate[A](length: Int)(f: Int => A): immutable.Vec[A] =
    immutable.Vec.tabulate(length)(f)

  def defaultTabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): immutable.Mat[A] =
    immutable.Mat.tabulate(rows, cols)(f)

}
