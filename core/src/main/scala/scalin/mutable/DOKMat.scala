package scalin
package mutable

import scala.collection.mutable.LongMap

import spire.syntax.cfor._
import scalin.syntax.all._

class DOKMat[A](val nRows: Int, val nCols: Int, val data: LongMap[AnyRef])(implicit A: Sparse[A]) extends mutable.Mat[A] {
  import DOKMat.index

  def sharedData: Boolean = false

  def prepareMutation(): Unit = ()

  def copyIfOverlap(obj: AnyRef): DOKMat[A] = if (obj eq this) new DOKMat[A](nRows, nCols, data.clone) else this

  def apply(r: Int, c: Int): A = data.getOrElse(index(r, c), A.zero.asInstanceOf[AnyRef]).asInstanceOf[A]

  def set(r: Int, c: Int, a: A): Unit =
    if (A.provenZero(a)) data.remove(index(r, c)) else data(index(r, c)) = a.asInstanceOf[AnyRef]
}

object DOKMat extends MatType[mutable.DOKMat] {

  def rowFromIndex(i: Long): Int = (i >> 32).toInt
  def colFromIndex(i: Long): Int = i.toInt
  def index(r: Int, c: Int): Long = (r.toLong << 32) + c

  type TC[A] = Sparse[A]

  class Engine[A](implicit val sparse: Sparse[A]) extends scalin.MatEngine[A, mutable.DOKMat[A]] { self =>
    type Ret = mutable.DOKMat[A]
    type Mut = DOKMat[A]
    def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): DOKMat[A] = {
      val res = new DOKMat[A](nRows, nCols, LongMap.empty[AnyRef])
      cforRange(0 until nCols) { c =>
        cforRange(0 until nRows) { r =>
          res(r, c) := f(r, c)
        }
      }
      res
    }

    def fillConstant(nRows: Int, nCols: Int)(a: => A): DOKMat[A] = {
      val v = a
      val res = new DOKMat[A](nRows, nCols, LongMap.empty[AnyRef])
      if (!sparse.provenZero(v)) {
        cforRange(0 until nCols) { c =>
          cforRange(0 until nRows) { r =>
            res(r, c) := v
          }
        }
      }
      res
    }

    def mutableEngine: scalin.MatEngine[A, DOKMat[A]] = self
    implicit def mutableConv: MatConv[A, DOKMat[A], DOKMat[A]] = new MatConv[A, DOKMat[A], DOKMat[A]] {
      def apply(from: DOKMat[A]): DOKMat[A] = new DOKMat[A](from.nRows, from.nCols, from.data.clone)
    }
  }

  def defaultEngine[A:TC]: scalin.MatEngine[A, mutable.DOKMat[A]] = new Engine[A]

  implicit def toCOO[A:SparseAdditiveGroup]: MatConv[A, DOKMat[A], COOMat[A]] = new DOKToCOO[A]
}
