package metascala

object Functions {
  trait TFn1B {
    type In
    type Out
    type Apply[T <: In] <: Out
  }

  trait TFn1[I, O] extends TFn1B {
    type In = I
    type Out = O
  }

  type ->[Arg, F <: TFn1[Arg, _]] = F#Apply[Arg]

  trait >>[F1 <: TFn1[_, _], F2 <: TFn1[_, _]] extends TFn1[F1#In, F2#Out] {
    type Apply[T] = F2#Apply[F1#Apply[T]]
  }
}
