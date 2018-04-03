package scalin

trait VecConv[A, -From <: Vec[A], +To <: Vec[A]] { self =>
  def apply(from: From): To
  def andThen[AndTo <: Vec[A]](ev: VecConv[A, To, AndTo]): VecConv[A, From, AndTo] =
    new VecConv[A, From, AndTo] {
      def apply(from: From): AndTo = ev.apply(self.apply(from))
    }
}

object VecConv {
  implicit def fromEngine[A, From <: Vec[A], To <: Vec[A]](implicit engine: VecEngine[A, To]): VecConv[A, From, To] =
    new VecConv[A, From, To] {
      def apply(from: From): To = engine.fromVec(from)
    }
}
