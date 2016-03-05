package scalin
package mutable

import algebra._
import spire.algebra._

// copy/paste of scalin/ops.scala

abstract class TraitOps {

  implicit def vecTrait[A]: VecTrait.Aux[A, Vec, Dummy] = new VecTrait[A, Vec] {
    def scalar = null
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

}

abstract class RingOps extends TraitOps {

    implicit def vecRing[A](implicit A: Ring[A]): VecRing[A, Vec] = new VecRing[A, Vec] {
    def scalar = A
    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Vec
  }

  implicit def matRing[A](implicit A: Ring[A]): MatRing[A, Mat] = new MatRing[A, Mat] {
    def scalar = A
    type Extra[A] = Dummy[A]
    def extra: Extra[A] = null
    def factory = Mat
  }

}

object ops extends RingOps
