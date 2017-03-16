package scalin

package object mutable {

  type MatEngine[A] = scalin.MatEngine[A, mutable.Mat[A]]

  type VecEngine[A] = scalin.VecEngine[A, mutable.Vec[A]]

}
