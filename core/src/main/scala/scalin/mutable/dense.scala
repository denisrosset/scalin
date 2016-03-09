package scalin
package mutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.impl.builder.VecEngine[A, mutable.DenseVec[A]] {

  type UA = mutable.DenseVec[A]
  def UA = this

  def tabulate(length: Int)(f: Int => A) = mutable.DenseVec.tabulate[A](length)(f)

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(vec: UA) = vec

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

class VecMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A])
    extends scalin.mutable.VecEngine[A]
    with scalin.impl.builder.VecMultiplicativeMonoid[A, mutable.DenseVec[A]]

class MatMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A],
  override val UVA: scalin.algebra.VecMultiplicativeMonoid[A, mutable.DenseVec[A]])
    extends scalin.mutable.MatEngine[A]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class VecRing[A](implicit override val scalar: Ring[A])
    extends scalin.mutable.VecMultiplicativeMonoid[A]
    with scalin.impl.builder.VecRing[A, mutable.DenseVec[A]]

class MatRing[A](implicit override val scalar: Ring[A],
  override val UVA: scalin.algebra.VecRing[A, mutable.DenseVec[A]])
    extends scalin.mutable.MatMultiplicativeMonoid[A]
    with scalin.impl.builder.MatRing[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class VecEuclideanRing[A](implicit override val scalar: EuclideanRing[A])
    extends scalin.mutable.VecRing[A]
    with scalin.impl.builder.VecEuclideanRing[A, mutable.DenseVec[A]]

class MatEuclideanRing[A](implicit override val scalar: EuclideanRing[A],
  override val UVA: scalin.algebra.VecEuclideanRing[A, mutable.DenseVec[A]],
  val pivotA: Pivot[A])
    extends scalin.mutable.MatRing[A]
    with scalin.impl.builder.MatEuclideanRing[A, mutable.DenseMat[A]] {

  override def UMA = this

}

class VecField[A](implicit override val scalar: Field[A])
    extends scalin.mutable.VecEuclideanRing[A]
    with scalin.impl.builder.VecField[A, mutable.DenseVec[A]]

class MatField[A](implicit override val scalar: Field[A],
  override val UVA: scalin.algebra.VecField[A, mutable.DenseVec[A]],
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

  implicit def vecMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.VecMultiplicativeMonoid[A, mutable.DenseVec[A]] =
    new VecMultiplicativeMonoid[A]

  implicit def matMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.MatMultiplicativeMonoid[A, mutable.DenseMat[A]] =
    new MatMultiplicativeMonoid[A]

}

abstract class dense2 extends dense1 {

  implicit def vecRing[A:Ring]: scalin.algebra.VecRing[A, mutable.DenseVec[A]] =
    new VecRing[A]

  implicit def matRing[A:Ring]: scalin.algebra.MatRing[A, mutable.DenseMat[A]] =
    new MatRing[A]

}

abstract class dense3 extends dense2 {

  implicit def vecEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.VecEuclideanRing[A, mutable.DenseVec[A]] =
    new VecEuclideanRing[A]

  implicit def matEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.MatEuclideanRing[A, mutable.DenseMat[A]] =
    new MatEuclideanRing[A]

}

object dense extends dense3 {

  implicit def vecField[A:Field:Pivot]: scalin.algebra.VecField[A, mutable.DenseVec[A]] =
    new VecField[A]

  implicit def matField[A:Field:Pivot]: scalin.algebra.MatField[A, mutable.DenseMat[A]] =
    new MatField[A]

}
