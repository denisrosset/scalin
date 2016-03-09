package scalin
package immutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.impl.builder.VecEngine[A, immutable.DenseVec[A]] {

  type UA = mutable.DenseVec[A]
  def UA = mutable.dense.vecEngine[A]

  def tabulate(length: Int)(f: Int => A) = immutable.DenseVec.tabulate[A](length)(f)

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(vec: UA) = vec.result()

}

class MatEngine[A] extends scalin.impl.builder.MatEngine[A, immutable.DenseMat[A]] {

  type UVA = mutable.DenseVec[A]
  def UVA = mutable.dense.vecEngine[A]

  type UMA = mutable.DenseMat[A]
  def UMA = mutable.dense.matEngine[A]

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => A) = immutable.DenseMat.tabulate[A](rows, cols)(f)

  def alloc(rows: Int, cols: Int) = new mutable.DenseMat(rows, cols, new Array[AnyRef](rows * cols))

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(mat: UMA) = mat.result()

}

class VecMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A])
    extends scalin.immutable.VecEngine[A]
    with scalin.impl.builder.VecMultiplicativeMonoid[A, immutable.DenseVec[A]] {

  override def UA = mutable.dense.vecMultiplicativeMonoid[A]

}

class MatMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A])
    extends scalin.immutable.MatEngine[A]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, immutable.DenseMat[A]] {

  override def UVA = mutable.dense.vecMultiplicativeMonoid[A]
  override def UMA = mutable.dense.matMultiplicativeMonoid[A]

}

class VecRing[A](implicit override val scalar: Ring[A])
    extends scalin.immutable.VecMultiplicativeMonoid[A]
    with scalin.impl.builder.VecRing[A, immutable.DenseVec[A]] {

  override def UA = mutable.dense.vecRing[A]

}

class MatRing[A](implicit override val scalar: Ring[A])
    extends scalin.immutable.MatMultiplicativeMonoid[A]
    with scalin.impl.builder.MatRing[A, immutable.DenseMat[A]] {

  override def UVA = mutable.dense.vecRing[A]
  override def UMA = mutable.dense.matRing[A]

}

class VecEuclideanRing[A](implicit override val scalar: EuclideanRing[A], val pivotA: Pivot[A])
    extends scalin.immutable.VecRing[A]
    with scalin.impl.builder.VecEuclideanRing[A, immutable.DenseVec[A]] {

  override def UA = mutable.dense.vecEuclideanRing[A]

}

class MatEuclideanRing[A](implicit override val scalar: EuclideanRing[A], val pivotA: Pivot[A])
    extends scalin.immutable.MatRing[A]
    with scalin.impl.builder.MatEuclideanRing[A, immutable.DenseMat[A]] {

  override def UVA = mutable.dense.vecEuclideanRing[A]
  override def UMA = mutable.dense.matEuclideanRing[A]

}

class VecField[A](implicit override val scalar: Field[A], override val pivotA: Pivot[A])
    extends scalin.immutable.VecEuclideanRing[A]
    with scalin.impl.builder.VecField[A, immutable.DenseVec[A]] {

  override def UA = mutable.dense.vecField[A]

}

class MatField[A](implicit override val scalar: Field[A], override val pivotA: Pivot[A])
    extends scalin.immutable.MatEuclideanRing[A]
    with scalin.impl.builder.MatField[A, immutable.DenseMat[A]] {

  override def UVA = mutable.dense.vecField[A]
  override def UMA = mutable.dense.matField[A]

}

abstract class dense0 {

  implicit def vecEngine[A]: scalin.algebra.VecEngine[A, immutable.DenseVec[A]] =
    new VecEngine[A]

  implicit def matEngine[A]: scalin.algebra.MatEngine[A, immutable.DenseMat[A]] =
    new MatEngine[A]

}

abstract class dense1 extends dense0 {

  implicit def vecMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.VecMultiplicativeMonoid[A, immutable.DenseVec[A]] =
    new VecMultiplicativeMonoid[A]

  implicit def matMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.MatMultiplicativeMonoid[A, immutable.DenseMat[A]] =
    new MatMultiplicativeMonoid[A]

}

abstract class dense2 extends dense1 {

  implicit def vecRing[A:Ring]: scalin.algebra.VecRing[A, immutable.DenseVec[A]] =
    new VecRing[A]

  implicit def matRing[A:Ring]: scalin.algebra.MatRing[A, immutable.DenseMat[A]] =
    new MatRing[A]

}

abstract class dense3 extends dense2 {

  implicit def vecEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.VecEuclideanRing[A, immutable.DenseVec[A]] =
    new VecEuclideanRing[A]

  implicit def matEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.MatEuclideanRing[A, immutable.DenseMat[A]] =
    new MatEuclideanRing[A]

}

object dense extends dense3 {

  implicit def vecField[A:Field:Pivot]: scalin.algebra.VecField[A, immutable.DenseVec[A]] =
    new VecField[A]

  implicit def matField[A:Field:Pivot]: scalin.algebra.MatField[A, immutable.DenseMat[A]] =
    new MatField[A]

}
