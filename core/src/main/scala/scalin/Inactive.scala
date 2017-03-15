package scalin

/** Provides a way to pass implicit instances with a wrapper that inactivates them. */
class Inactive[F](val f: F) extends AnyVal

object Inactive {

  implicit def inactive[F](implicit f: F): Inactive[F] = new Inactive[F](f)

}
