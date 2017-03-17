package scalin

trait Dummy[A]

object Dummy {

  implicit def dummy[A]: Dummy[A] = null

}
