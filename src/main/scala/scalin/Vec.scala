package scalin

import spire.algebra._

class PointwiseVec[A](val lhs: Vec[A]) extends AnyVal {

  def +[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.pointwisePlus(lhs, rhs)

}

/** Concrete matrix trait. */
trait Vec[A] { lhs =>

  override def toString: String = Printer.vec(Vec.this)

  def length: Int

  def apply(k: Int): A

  def pointwisePlus[V[A] <: Vec[A]](rhs: A)(implicit ev: VecRing[A, V]): V[A] = ev.pointwisePlus(lhs, rhs)
//  def pointwisePlus(rhs: A)(implicit A: AdditiveSemigroup[A]): Vec[A]  = factory.tabulate(length)(k => apply(k) + rhs)

  @inline def pointwise: PointwiseVec[A] = new PointwiseVec[A](lhs)

}

object Vec extends VecFactory[Vec, Dummy] {

  def tabulate[A:Dummy](length: Int)( f: Int => A ): Vec[A] =
    immutable.DenseVec.tabulate[A](length)(f)

  implicit def ring[A:Ring]: VecRing[A, Vec] = new VecRing[A, Vec] {

    def ring = implicitly[Ring[A]]

    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Vec

  }

}

trait VecRing[A, V[A] <: Vec[A]] {

  implicit def ring: Ring[A]

  type Extra[_]
  implicit def extra: Extra[A]
  def factory: VecFactory[V, Extra]

  import spire.syntax.ring._

  def plus(lhs: Vec[A], rhs: Vec[A]): V[A] = factory.tabulate(lhs.length)(k => lhs(k) + rhs(k))

  def pointwisePlus(lhs: Vec[A], rhs: A): V[A] = factory.tabulate(lhs.length)(k => lhs(k) + rhs)

}
