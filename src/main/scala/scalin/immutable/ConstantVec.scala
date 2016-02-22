package scalin
package immutable

case class ConstantVec[A](a: A, length: Int) extends immutable.Vec[A] {

  def apply(k: Int) = a

  def nextNonZero(k: Int) = k + 1

  override def touch(node: AbstractNode) = Touch.Clean()

}
