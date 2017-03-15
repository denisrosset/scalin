package scalin

trait VecType[V[A] <: Vec[A]] {

  type TC[_]

  def engine[A:TC]: VecEngine[A, V[A]]

  def empty[A:TC]: V[A] = engine[A].empty

  def tabulate[A:TC](length: Int)(f: Int => A): V[A] = engine[A].tabulate(length)(f)

  def fill[A:TC](length: Int)(f: => A): V[A] = engine[A].fill(length)(f)

  def fillConstant[A:TC](length: Int)(a: A): V[A] = engine[A].fill(length)(a)

  def fromSeq[A:TC](elements: Seq[A]): V[A] = engine[A].fromSeq(elements)

  def fromVec[A:TC](vec: Vec[A]): V[A] = engine[A].fromVec(vec)

}
