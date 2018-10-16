package scalin.decomposition

object Epsilon {
  def apply[A](implicit ev: Epsilon[A]): Epsilon[A] = ev
  val tinyDouble = spire.math.pow(2.0, -966.0)
  implicit val double: Epsilon[Double] = Epsilon(spire.math.pow(2.0, -52.0), tinyDouble)
  //implicit val doubleDouble: Epsilon[DoubleDouble] = Epsilon(DoubleDouble(DoubleDouble.eps), DoubleDouble(tinyDouble))
}

case class Epsilon[A](val eps: A, val tiny: A)
