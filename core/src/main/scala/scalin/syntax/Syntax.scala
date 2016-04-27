package scalin
package syntax

trait AssignSyntax {

  implicit def assignOps[A](lhs: A): AssignOps[A] = new AssignOps[A](lhs)

}

/* We split the type specification is two parts, so that the constructed type `VA <: Vec[A]`
 * can be inferred from whatever implicit type class for `Vec[A]` is available when writing
 * e.g. build.zeros[A](3); also, we choose not to return path-dependent types, they would lead
 * to ugly types in the REPL.
 */
trait BuildSyntax {

  /** Matrix/vector full of zeroes. */
  implicit def zeros[A]: ZerosOps[A] = new ZerosOps[A](null)

  /** Matrix/vector full of ones. */
  implicit def ones[A]: OnesOps[A] = new OnesOps[A](null)

  /** Identity matrix. */
  implicit def eye[A]: EyeOps[A] = new EyeOps[A](null)

  /** Fill operation. */
  implicit def fill[A]: FillOps[A] = new FillOps[A](null)

  /** Vector builder. */
  implicit def vec[A]: VecOps[A] = new VecOps[A](null)

  /** Matrix builder using tuples (max. 22 columns). */
  implicit def mat[A]: MatOps[A] = new MatOps[A](null)

  /** Tabulate operation. */
  implicit def tabulate[A]: TabulateOps[A] = new TabulateOps[A](null)

  /** Matrix builder with row-major data. */
  implicit def rowMajor[A]: RowMajorOps[A] = new RowMajorOps[A](null)

  /** Matrix builder with column-major data. */
  implicit def colMajor[A]: ColMajorOps[A] = new ColMajorOps[A](null)

  /** Row matrix builder. */
  implicit def rowMat[A]: RowMatOps[A] = new RowMatOps[A](null)

  /** Col matrix builder. */
  implicit def colMat[A]: ColMatOps[A] = new ColMatOps[A](null)

}

trait AllSyntax
    extends AssignSyntax
    with BuildSyntax
