package scalin

package object immutable {

  type MatEngine[A] = scalin.MatEngine[A, immutable.Mat[A]]

  type VecEngine[A] = scalin.VecEngine[A, immutable.Vec[A]]

  def MatEngine[A](implicit ev: MatEngine[A]): MatEngine[A] = ev

  def VecEngine[A](implicit ev: VecEngine[A]): VecEngine[A] = ev

}
