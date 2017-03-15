package scalin
package immutable

import spire.algebra.{MultiplicativeMonoid, Ring, EuclideanRing, Field}

import scalin.algebra.Pivot

class VecEngine[A] extends scalin.algebra.VecEngine[A, immutable.DenseVec[A]] {

  def tabulate(length: Int)(f: Int => A) = immutable.DenseVec.tabulate[A](length)(f)

  def fromMutable(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit) = {
    val res = new scalin.mutable.DenseVec[A](new Array[AnyRef](length)) // add method to mutable VecEngine to allocate vector with null, sound semantics; see mutable.VecEngine as well
    updateFun(res)
    res.result()
  }

}

class MatEngine[A] extends scalin.impl.builder.MatEngine[A, immutable.DenseMat[A]] {

  type UVA = mutable.DenseVec[A]
  def UVA = mutable.dense.vecEngine[A]

  type UMA = mutable.DenseMat[A]
  def UMA = mutable.dense.matEngine[A]

  def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A) = immutable.DenseMat.tabulate[A](nRows, nCols)(f)

  def fromMutable(nRows: Int, nCols: Int)(updateFun: scalin.mutable.Mat[A] => Unit) = {
    val res = new mutable.DenseMat[A](nRows, nCols, new Array[AnyRef](nRows * nCols))
    // TODO: zero semantics
    updateFun(res)
    res.result()
  }

  def alloc(rows: Int, cols: Int) = new mutable.DenseMat(rows, cols, new Array[AnyRef](rows * cols))

  def alloc(length: Int) = new mutable.DenseVec[A](new Array[AnyRef](length))

  def result(mat: UMA) = mat.result()

}

class MatMultiplicativeMonoid[A](implicit val scalar: MultiplicativeMonoid[A])
    extends scalin.immutable.MatEngine[A]
    with scalin.impl.builder.MatMultiplicativeMonoid[A, immutable.DenseMat[A]] {

  override def UMA = mutable.dense.matMultiplicativeMonoid[A]

}

class MatRing[A](implicit override val scalar: Ring[A])
    extends scalin.immutable.MatMultiplicativeMonoid[A]
    with scalin.impl.builder.MatRing[A, immutable.DenseMat[A]] {

  override def UMA = mutable.dense.matRing[A]

}

class MatEuclideanRing[A](implicit override val scalar: EuclideanRing[A], val pivotA: Pivot[A])
    extends scalin.immutable.MatRing[A]
    with scalin.impl.builder.MatEuclideanRing[A, immutable.DenseMat[A]] {

  override def UMA = mutable.dense.matEuclideanRing[A]

}

class MatField[A](implicit override val scalar: Field[A], override val pivotA: Pivot[A])
    extends scalin.immutable.MatEuclideanRing[A]
    with scalin.impl.builder.MatField[A, immutable.DenseMat[A]] {

  override def UMA = mutable.dense.matField[A]

}

abstract class dense0 {

  implicit def vecEngine[A]: scalin.algebra.VecEngine[A, immutable.DenseVec[A]] =
    new VecEngine[A]

  implicit def matEngine[A]: scalin.algebra.MatEngine[A, immutable.DenseMat[A]] =
    new MatEngine[A]

}

abstract class dense1 extends dense0 {

  implicit def matMultiplicativeMonoid[A:MultiplicativeMonoid]: scalin.algebra.MatMultiplicativeMonoid[A, immutable.DenseMat[A]] =
    new MatMultiplicativeMonoid[A]

}

abstract class dense2 extends dense1 {

  implicit def matRing[A:Ring]: scalin.algebra.MatRing[A, immutable.DenseMat[A]] =
    new MatRing[A]

}

abstract class dense3 extends dense2 {

  implicit def matEuclideanRing[A:EuclideanRing:Pivot]: scalin.algebra.MatEuclideanRing[A, immutable.DenseMat[A]] =
    new MatEuclideanRing[A]

}

object dense extends dense3 {

  implicit def matField[A:Field:Pivot]: scalin.algebra.MatField[A, immutable.DenseMat[A]] =
    new MatField[A]

}
