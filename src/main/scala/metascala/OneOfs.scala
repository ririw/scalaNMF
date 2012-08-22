package metascala


import reflect.Manifest

object OneOfs {
  import TLists._

  case class OneOf[TS <: TList](value : Any)


  implicit def toOneOf[H, H2, T <: TList](value : H)(implicit fn : ContainsFn[H, H2 :: T]) : OneOf[H2 :: T] = OneOf[H2 :: T](value)
  implicit def convertOneOf[T1 <: TList, T2 <: TList](ts : OneOf[T1])(implicit fn : IsSubSetFn[T1, T2]) = OneOf[T2](ts.value)


  def doMatch[T <: TList](ts : OneOf[T]) = NoMatch[T, Any](ts.value)

  trait Matcher[TS <: TList, RT] {
    def doIf[T2, RT2 <: RT, TS2 <: TList](fn : T2 => RT2)(implicit rfn : RemoveFirstFn[T2, TS, TS2], m : Manifest[T2]) : Matcher[TS2, RT]
    def apply() : Option[RT]
  }

  final case class Match[TS <: TList, RT](result : RT) extends Matcher[TS, RT] {
    def doIf[T2, RT2 <: RT, TS2 <: TList](fn : T2 => RT2)(implicit rfn : RemoveFirstFn[T2, TS, TS2], m : Manifest[T2]) = Match[TS2, RT](result)
    def apply() = Some(result)
  }

  final case class NoMatch[TS <: TList, RT](value : Any) extends Matcher[TS, RT] {
    def doIf[T2, RT2 <: RT, TS2 <: TList](fn : T2 => RT2)(implicit rfn : RemoveFirstFn[T2, TS, TS2], m : Manifest[T2]) =
      if (m.erasure.isInstance(value)) Match[TS2, RT](fn(value.asInstanceOf[T2])) else NoMatch[TS2, RT](value)

    def apply() = None
  }

}
