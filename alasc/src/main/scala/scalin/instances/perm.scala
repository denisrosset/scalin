package scalin
package instances

import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm
import spire.algebra.{Action, Group, Order}
import spire.syntax.group._

final class PermVecAction[A, VA <: Vec[A], G](implicit gG: Group[G], paG: PermutationAction[G], VA: VecEngine[A, VA]) extends Action[VA, G] {
    import spire.syntax.action._
    import spire.syntax.group._
    import net.alasc.syntax.permutationAction._
    def actl(g: G, v: VA): VA = {
      require(g.largestMovedPoint.getOrElseFast(-1) < v.length)
      VA.tabulate(v.length)( k => v(k <|+| g ))
    }
    def actr(v: VA, g: G): VA = actl(g.inverse, v)
}

object RichVecPerm {
  def swap(indices: Array[Int], i: Int, j: Int): Unit = {
    val temp = indices(i)
    indices(i) = indices(j)
    indices(j) = temp
  }

  @inline final def limit: Int = 16

  def insertionSort[A: Order](v: Vec[A], indices: Array[Int], left: Int, right: Int): Unit = {
    var i = left + 1
    while (i <= right) {
      val item = indices(i)
      var hole = i
      while (hole > left && Order[A].gt(v(indices(hole - 1)), v(item))) {
        indices(hole) = indices(hole - 1)
        hole -= 1
      }
      indices(hole) = item
      i += 1
    }
  }

  def quickSort[A: Order](v: Vec[A], indices: Array[Int], left: Int, right: Int): Unit = {
    def partition(pivot: Int): Int = {
      val value = v(indices(pivot))

      swap(indices, pivot, right)

      var store = left
      var i = left
      while (i < right) {
        if (Order[A].lt(v(indices(i)), value)) {
          swap(indices, i, store)
          store += 1
        }
        i += 1
      }
      swap(indices, store, right)
      store
    }

    if (right - left < limit) return insertionSort(v, indices, left, right)
    val pivot = left + (right - left) / 2
    val next = partition(pivot)
    quickSort(v, indices, left, next - 1)
    quickSort(v, indices, next + 1, right)
  }
}

final class RichVecPerm[A, VA <: Vec[A]](val lhs: VA) extends AnyVal {

  def sortingPerm(implicit A: Order[A]): Perm = {
    val indices = Array.range(0, lhs.length)
    RichVecPerm.quickSort(lhs, indices, 0, lhs.length - 1)
    Perm.fromImages(indices).inverse
  }

}

trait PermInstances {
  implicit def richVecPerm[A, VA <: Vec[A]](v: VA with Vec[A]): RichVecPerm[A, VA] =
    new RichVecPerm[A, VA](v)

  implicit def permVecAction[V[X] <: Vec[X], A, G:Group:PermutationAction](implicit VA: VecEngine[A, V[A]]): Action[V[A], G] = {
    new PermVecAction[A, V[A], G]
  }
}
