package scalin

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Rig}
import spire.NoImplicit

class MatBuilder[A, MA <: Mat[A]](val engine: MatEngine[A, MA]) extends AnyVal

trait MatType[M[A] <: Mat[A]] {

  type TC[_]

  type Builder[A] = MatBuilder[A, M[A]]

  def Builder[A](implicit ev: MatBuilder[A, M[A]]): Builder[A] = ev

  def defaultEngine[A:TC]: MatEngine[A, M[A]]

  implicit def defaultBuilder[A:TC](implicit NI: NoImplicit[MatEngine[A, M[A]]]): MatBuilder[A, M[A]] = new MatBuilder[A, M[A]](defaultEngine[A])

  implicit def fromMatEngine[A](implicit MA: MatEngine[A, M[A]]): MatBuilder[A, M[A]] = new MatBuilder[A, M[A]](MA)

  def tabulate[A:Builder](nRows: Int, nCols: Int)(f: (Int, Int) => A): M[A] = Builder[A].engine.tabulate(nRows, nCols)(f)

  def zeros[A:Builder:AdditiveMonoid](nRows: Int, nCols: Int): M[A] = Builder[A].engine.zeros(nRows, nCols)

  def ones[A:Builder:MultiplicativeMonoid](nRows: Int, nCols: Int): M[A] = Builder[A].engine.ones(nRows, nCols)

  def eye[A:Builder:Rig](n: Int): M[A] = Builder[A].engine.eye(n)

  def fill[A:Builder](nRows: Int, nCols: Int)(f: => A): M[A] = Builder[A].engine.fill(nRows, nCols)(f)

  def fillConstant[A:Builder](nRows: Int, nCols: Int)(a: A): M[A] = Builder[A].engine.fillConstant(nRows, nCols)(a)

  def sparse[A:Builder:Sparse](nRows: Int, nCols: Int)(i: Vec[Int], j: Vec[Int], v: Vec[A]): M[A] =
    Builder[A].engine.sparse(nRows, nCols)(i, j, v)

  def colMajor[A:Builder](rows: Int, cols: Int)(elements: A*): M[A] = Builder[A].engine.colMajor(rows, cols)(elements: _*)

  def rowMajor[A:Builder](rows: Int, cols: Int)(elements: A*): M[A] = Builder[A].engine.rowMajor(rows, cols)(elements: _*)

  def colMat[A:Builder](elements: A*): M[A] = Builder[A].engine.colMat(elements: _*)

  def rowMat[A:Builder](elements: A*): M[A] = Builder[A].engine.rowMat(elements: _*)

  protected def iterate[A:Builder](first: Product, next: Product*): M[A] =
    Builder[A].engine.tabulate(next.size + 1, first.productArity) { (r, c) =>
      if (r == 0) first.productElement(c).asInstanceOf[A] else next(r- 1).productElement(c).asInstanceOf[A]
    }

  def apply[A:Builder](first: (A, A), next: (A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A), next: (A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A), next: (A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A), next: (A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A), next: (A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)
  
  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)
  
  // 11 to 20

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  // 21 to 22

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)

  def apply[A:Builder](first: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A), next: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)*): M[A] = iterate[A](first, next: _*)


}
