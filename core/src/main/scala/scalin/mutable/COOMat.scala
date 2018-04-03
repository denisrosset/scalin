package scalin
package mutable

import spire.algebra.{AdditiveGroup, AdditiveMonoid, AdditiveSemigroup}
import spire.syntax.cfor._
import spire.syntax.additiveGroup._

import scala.annotation.tailrec

/** A sparse matrix type whose nonzero elements are stored as unsorted triplets (row, col, value).
  *
  * Duplicate elements are allowed.
  *
  * The only fast operation is `add`, while indexing and updating is linear in the number
  * of elements.
  */
class COOMat[A](val nRows: Int, val nCols: Int,
                var data: Array[AnyRef],
                var rowIndices: Array[Int],
                var colIndices: Array[Int],
                var n: Int)(implicit val sparse: SparseAdditiveGroup[A]) extends mutable.Mat[A] {

  def sharedData: Boolean = false

  def prepareMutation(): Unit = ()

  implicit def additiveGroup: AdditiveGroup[A] = sparse.additive

  /** Sorts the elements by column first, then by row. Does not compact duplicates. */
  def shellSort(): Unit = {
    prepareMutation()
    @inline def less(x: Int, y: Int): Boolean = java.lang.Integer.compare(colIndices(x), colIndices(y)) match {
      case -1 => true
      case 0 => rowIndices(x) < rowIndices(y)
      case 1 => false
    }

    @inline def swap[@specialized T](a: Array[T], i: Int, j: Int): Unit = {
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }

    var h = 1
    while (h < n / 3) h = 3 * h + 1
    while (h >= 1) {
      cforRange(h until n) { i =>
        var j = i
        while (j >= h && less(j, j - h)) {
          swap(data, j, j - h)
          swap(rowIndices, j, j - h)
          swap(colIndices, j, j - h)
          j -= h
        }
      }
      h /= 3
    }
  }

  /** Compacts successive elements with same (row, col). */
  def compact(): Unit = {
    prepareMutation()
    @tailrec def rec(i: Int, // current empty storage position
                     r: Int, // current row
                     c: Int, // current col
                     a: A, // current sum
                     j: Int // element to inspect
                    ): Int =
      if (j == n || r != rowIndices(j) || c != colIndices(j)) {
        if (Sparse[A].provenZero(a)) {
          if (j == n)
            i
          else
            rec(i, rowIndices(j), colIndices(j), data(j).asInstanceOf[A], j + 1) // reuse storage position i
        } else {
          rowIndices(i) = r
          colIndices(i) = c
          data(i) = a.asInstanceOf[AnyRef]
          if (j == n)
            i + 1 // number of stored elements
          else
            rec(i + 1, rowIndices(j), colIndices(j), data(j).asInstanceOf[A], j + 1)
        }
      } else rec(i, r, c, a + data(j).asInstanceOf[A], j + 1)

    if (n > 0) {
      n = rec(0, rowIndices(0), colIndices(0), data(0).asInstanceOf[A], 1)
    }
  }

  def apply(r: Int, c: Int): A = {
    if (r >= nRows || c >= nCols || r < 0 || c < 0)
      throw new IndexOutOfBoundsException()
    var s = additiveGroup.zero
    cforRange(0 until n) { i =>
      if (rowIndices(i) == r && colIndices(i) == c)
        s = s + data(i).asInstanceOf[A]
    }
    s
  }


  def set(r: Int, c: Int, a: A): Unit = add(r, c, a - apply(r, c))

  override def add(r: Int, c: Int, a: A)(implicit ev: AdditiveSemigroup[A]): Unit =
    if (!sparse.provenZero(a)) {
      prepareMutation()
      if (n == data.length) {
        val newL = newLength(data.length)

        // allocate new arrays
        data = java.util.Arrays.copyOf(data, newL)
        rowIndices = java.util.Arrays.copyOf(rowIndices, newL)
        colIndices = java.util.Arrays.copyOf(colIndices, newL)
      }
      data(n) = a.asInstanceOf[AnyRef]
      rowIndices(n) = r
      colIndices(n) = c
      n += 1
    }

  def copyIfOverlap(obj: AnyRef): scalin.Mat[A] = new COOMat[A](nRows, nCols, data.clone, rowIndices.clone, colIndices.clone, n)
}

object COOMat extends MatType[mutable.COOMat] {

  type TC[X] = SparseAdditiveGroup[X]

  class Engine[A](implicit val sparse: SparseAdditiveGroup[A]) extends scalin.MatEngine[A, mutable.COOMat[A]] {
    self =>
    implicit def additiveGroup: AdditiveGroup[A] = sparse.additive
    type Mut = mutable.COOMat[A]
    type Ret = mutable.COOMat[A]
    def mutableEngine: scalin.MatEngine[A, mutable.COOMat[A]] = self
    implicit def mutableConv: MatConv[A, mutable.COOMat[A], mutable.COOMat[A]] =
      new MatConv[A, mutable.COOMat[A], mutable.COOMat[A]] {
        def apply(from: mutable.COOMat[A]): mutable.COOMat[A] =
          new mutable.COOMat[A](from.nRows, from.nCols, from.data, from.rowIndices.clone, from.colIndices.clone, from.n)
      }

    protected def newArray(size: Int, default: => A): Array[AnyRef] = {
      val res = new Array[AnyRef](size)
      if (size > 0)
        java.util.Arrays.fill(res, default)
      res
    }


    def tabulate(nRows: Int, nCols: Int)(f: (Int, Int) => A): COOMat[A] = {
      val res = new COOMat[A](nRows, nCols, new Array[AnyRef](0), new Array[Int](0), new Array[Int](0), 0)
      cforRange(0 until nCols) { c =>
        cforRange(0 until nRows) { r =>
          res.add(r, c, f(r, c))
        }
      }
      res
    }

    protected def build(nRows: Int, nCols: Int,
                        data: Array[AnyRef],
                        rowIndices: Array[Int],
                        colIndices: Array[Int],
                        nnz: Int): mutable.COOMat[A] =
      new COOMat[A](nRows, nCols, data, rowIndices, colIndices, nnz)

    override def fillConstant(nRows: Int, nCols: Int)(a: => A): mutable.COOMat[A] = {
      val value = a
      if (sparse.provenZero(a) || nRows == 0 || nCols == 0)
        build(nRows, nCols, new Array[AnyRef](0), new Array[Int](0), new Array[Int](0), 0)
      else {
        val rowIndices = new Array[Int](nRows * nCols)
        val colIndices = new Array[Int](nRows * nCols)
        val data = new Array[AnyRef](nRows * nCols)
        java.util.Arrays.fill(data, value)
        cforRange(0 until nCols) { c => colIndices(c) = c }
        cforRange(1 until nRows) { r => Array.copy(colIndices, 0, colIndices, r * nCols, nCols) }
        cforRange(0 until nRows) { r =>
          java.util.Arrays.fill(rowIndices, r * nCols, (r + 1) * nCols, r)
        }
        build(nRows, nCols, data, rowIndices, colIndices, nRows * nCols)
      }
    }

    override def sparse(nRows: Int, nCols: Int)(i: scalin.Vec[Int], j: scalin.Vec[Int], v: scalin.Vec[A])
                       (implicit A: AdditiveMonoid[A], sparse: Sparse[A]): mutable.COOMat[A] = {
      val n = i.length
      assert(n == j.length)
      assert(n == v.length)
      val rowArray = Array.tabulate(n)(i(_))
      val colArray = Array.tabulate(n)(j(_))
      val data = Array.tabulate(n)(v(_).asInstanceOf[AnyRef])
      build(nRows, nCols, data, rowArray, colArray, n)
    }
  }

  def defaultEngine[A: SparseAdditiveGroup]: scalin.MatEngine[A, mutable.COOMat[A]] = new Engine[A]
}
