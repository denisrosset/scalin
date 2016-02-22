package scalin

trait Dummy[A]

object Dummy {

  implicit def instance[A]: Dummy[A] = null

}
