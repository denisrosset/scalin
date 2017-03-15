package scalin

trait MatType[M[A] <: Mat[A]] {

  type TC[_]

  def engine[A:TC]: MatEngine[A, M[A]]

  def tabulate[A:TC](nRows: Int, nCols: Int)(f: (Int, Int) => A): M[A] = engine[A].tabulate(nRows, nCols)(f)

  def fill[A:TC](nRows: Int, nCols: Int)(f: => A): M[A] = engine[A].fill(nRows, nCols)(f)

  def fillConstant[A:TC](nRows: Int, nCols: Int)(a: A): M[A] = engine[A].fillConstant(nRows, nCols)(a)

}
