import scalin.algebra._

package object scalin {

  /* We split the type specification is two parts, so that Vec[A] can be inferred from
   * whatever type class is available, *and* to avoid a path-dependent return type for
   * REPL purposes.
   */
  implicit def zeros[A]: Zeros[A] = new Zeros[A](null)

  implicit def ones[A]: Ones[A] = new Ones[A](null)

  implicit def eye[A]: Eye[A] = new Eye[A](null)

  final class Zeros[A](val dummy: Null) extends AnyVal {

    def apply[V[A] <: Vec[A]](length: Int)(implicit ev: VecRing[A, V]): V[A] = ev.zeros(length)

    def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, M]): M[A] = ev.zeros(rows, cols)

  }

  final class Ones[A](val dummy: Null) extends AnyVal {

    def apply[V[A] <: Vec[A]](length: Int)(implicit ev: VecRing[A, V]): V[A] = ev.ones(length)

    def apply[M[A] <: Mat[A]](rows: Int, cols: Int)(implicit ev: MatRing[A, M]): M[A] = ev.ones(rows, cols)

  }

  final class Eye[A](val dummy: Null) extends AnyVal {

    def apply[M[A] <: Mat[A]](n: Int)(implicit ev: MatRing[A, M]): M[A] = ev.eye(n)

  }

}
