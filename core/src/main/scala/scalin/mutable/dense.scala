package scalin
package mutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.algebra.VecEngine[A, mutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = mutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see immutable.VecEngine as well
    updateFun(res)
    res
  }

}

class MatEngine[A](implicit val UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]])
    extends scalin.impl.builder.MatEngine[A, mutable.DenseMat[A]] {

  type UMA = mutable.DenseMat[A]
  def UMA = this

  type UVA = mutable.DenseVec[A]

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A) = mutable.DenseMat.tabulate[A](rows, cols)(f)

  def alloc(rows: Int, cols: Int) = new mutable.DenseMat(rows, cols, new Array[AnyRef](rows * cols))

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(mat: UMA) = mat

}

class MatMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A],
  UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]])
    extends scalin.mutable.MatEngine[A]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class MatRing[A](implicit override val scalar: Ring[A],
  UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]])
    extends scalin.mutable.MatMultiplicativeMonoid[A]
    with scalin.impl.builder.MatRing[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class MatEuclideanRing[A](implicit override val scalar: EuclideanRing[A],
  UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]],
  val pivotA: Pivot[A])
    extends scalin.mutable.MatRing[A]
    with scalin.impl.builder.MatEuclideanRing[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class MatField[A](implicit override val scalar: Field[A],
  UVA: scalin.algebra.VecEngine[A, mutable.DenseVec[A]],
  override val pivotA: Pivot[A])
    extends scalin.mutable.MatEuclideanRing[A]
    with scalin.impl.builder.MatField[A, mutable.DenseMat[A]] {

  override def UMA = this

}

abstract class dense0 {

  implicit def vecEngine[A]: scalin.algebra.VecEngine[A, mutable.DenseVec[A]] =
    new VecEngine[A]

  implicit def matEngine[A]: scalin.algebra.MatEngine[A, mutable.DenseMat[A]] =
    new MatEngine[A]

}

abstract class dense1 extends dense0 {

  implicit def matMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.MatMultiplicativeMonoid[A, mutable.DenseMat[A]] =
    new MatMultiplicativeMonoid[A]

}

abstract class dense2 extends dense1 {

  implicit def matRing[A:Ring]: scalin.algebra.MatRing[A, mutable.DenseMat[A]] =
    new MatRing[A]

}

abstract class dense3 extends dense2 {

  implicit def matEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.MatEuclideanRing[A, mutable.DenseMat[A]] =
    new MatEuclideanRing[A]

}

object dense extends dense3 {

  implicit def matField[A:Field:Pivot]: scalin.algebra.MatField[A, mutable.DenseMat[A]] =
    new MatField[A]

}
