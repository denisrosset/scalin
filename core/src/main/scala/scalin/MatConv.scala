package scalin

/** Matrix type converter. */
trait MatConv[A, -From <: Mat[A], +To <: Mat[A]] { self =>
  /** Constructs a matrix of type `To` with the same content as the given matrix.
    *
    * Data sharing
    * From type  To Type      Behavior
    * Immutable  Immutable    Shared data
    * Mutable    Immutable    Shared data, `from` is marked as exported
    * Immutable  Mutable      Shared data, `to` is marked as exported
    * Mutable    Mutable      Data copy
    */
  def apply(from: From): To
  def andThen[AndTo <: Mat[A]](ev: MatConv[A, To, AndTo]): MatConv[A, From, AndTo] =
    new MatConv[A, From, AndTo] {
      def apply(from: From): AndTo = ev.apply(self.apply(from))
    }
}

object MatConv {
  implicit def fromEngine[A, From <: Mat[A], To <: Mat[A]](implicit engine: MatEngine[A, To]): MatConv[A, From, To] =
    new MatConv[A, From, To] {
      def apply(from: From): To = engine.fromMat(from)
    }
}
