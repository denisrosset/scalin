package scalin

import algebra._
import spire.algebra._

object ops {

  implicit def vecTrait[A]: VecTrait.Aux[A, Vec, Dummy] = new VecTrait[A, Vec] {

    def scalar = null

    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Vec

  }

  implicit def vecRing[A](implicit A: Ring[A]): VecRing[A, Vec] = new VecRing[A, Vec] {

    def scalar = A

    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Vec

  }

  implicit def matTrait[A]: MatTrait.Aux[A, Mat, Dummy] = new MatTrait[A, Mat] {

    def scalar = null

    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Mat

  }


  implicit def matRing[A](implicit A: Ring[A]): MatRing[A, Mat] = new MatRing[A, Mat] {

    def scalar = A

    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Mat

  }

}
