package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.NoImplicit

class VecBuilder[A, VA <: Vec[A]](val engine: VecEngine[A, VA]) extends AnyVal

trait VecType[V[A] <: Vec[A]] {

  type TC[_]

  type Builder[A] = VecBuilder[A, V[A]]

  def Builder[A](implicit ev: VecBuilder[A, V[A]]): Builder[A] = ev

  def defaultEngine[A:TC]: VecEngine[A, V[A]]

  implicit def defaultBuilder[A:TC](implicit NI: NoImplicit[VecEngine[A, V[A]]]): VecBuilder[A, V[A]] = {
    identity(NI)
    new VecBuilder[A, V[A]](defaultEngine[A])
  }

  implicit def fromVecEngine[A](implicit VA: VecEngine[A, V[A]]): VecBuilder[A, V[A]] = new VecBuilder[A, V[A]](VA)
  
  def empty[A:Builder]: V[A] = Builder[A].engine.empty

  def tabulate[A:Builder](length: Int)(f: Int => A): V[A] = Builder[A].engine.tabulate(length)(f)

  def tabulateBlocks[A:Builder](nBlocks: Int)(f: Int => Vec[A]): V[A] =
    Builder[A].engine.blockTabulate(nBlocks)(f)

  def fromMutable[A:Builder](length: Int, default: A)(updateFun: scalin.mutable.Vec[A] => Unit): V[A] = Builder[A].engine.fromMutable(length, default)(updateFun)

  def fill[A:Builder](length: Int)(f: => A): V[A] = Builder[A].engine.fill(length)(f)

  def fillConstant[A:Builder](length: Int)(a: A): V[A] = Builder[A].engine.fill(length)(a)

  def zeros[A:Builder:AdditiveMonoid](length: Int): V[A] = Builder[A].engine.zeros(length)

  def ones[A:Builder:MultiplicativeMonoid](length: Int): V[A] = Builder[A].engine.ones(length)

  def fromSeq[A:Builder](elements: Seq[A]): V[A] = Builder[A].engine.fromSeq(elements)

  def fromVec[A:Builder](vec: Vec[A]): V[A] = Builder[A].engine.fromVec(vec)

  def apply[A:Builder](elements: A*): V[A] = Builder[A].engine.fromSeq(elements)

}
