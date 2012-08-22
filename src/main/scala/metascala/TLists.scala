package metascala


object TLists {
  import Nats._
  import Utils._
  import Visitables._

  trait TListVisitor extends TypeVisitor {
    type VisitNil <: ResultType
    type VisitCons[H, T <: TList] <: ResultType
  }

  sealed trait TList extends Visitable[TListVisitor] {
    type Append[L <: TList] <: TList
    type ReverseAppend[L <: TList] <: TList
    type Length <: Nat
    type Nth[N <: Nat]
    type RemoveNth[N <: Nat] <: TList
    type Insert[N <: Nat, E] <: TList
    type Reverse = ReverseAppend[TNil]
  }

  final class TNil extends TList {
    type Accept[V <: TListVisitor] = V#VisitNil
    type Head = Invalid
    type Tail = TNil
    type Append[L <: TList] = L
    type ReverseAppend[L <: TList] = L
    type Length = _0
    type Nth[N <: Nat] = Invalid
    type RemoveNth[N <: Nat] = TNil

    class InsertVisitor[E] extends NatVisitor {
      type ResultType = TList
      type Visit0 = TCons[E, TNil]
      type VisitSucc[Pre <: Nat] = TNil
    }

    type Insert[N <: Nat, E] = N#Accept[InsertVisitor[E]]
  }

  final class TCons[H, T <: TList] extends TList {
    type Accept[V <: TListVisitor] = V#VisitCons[H, T]
    type This = TCons[H, T]
    type Head = H
    type Tail = T
    type Append[L <: TList] = TCons[H, T#Append[L]]
    type ReverseAppend[L <: TList] = Tail#ReverseAppend[TCons[H, L]]
    type Length = Succ[T#Length]

    class NthVisitor[H, T <: TList] extends NatVisitor {
      type ResultType = Any
      type Visit0 = H
      type VisitSucc[Pre <: Nat] = T#Nth[Pre]
    }

    type Nth[N <: Nat] = N#Accept[NthVisitor[H, T]]

    class RemoveNthVisitor[H, T <: TList] extends NatVisitor {
      type ResultType = TList
      type Visit0 = T
      type VisitSucc[Pre <: Nat] = TCons[H, T#RemoveNth[Pre]]
    }

    type RemoveNth[N <: Nat] = N#Accept[RemoveNthVisitor[H, T]]

    class InsertVisitor[H, T <: TList, E] extends NatVisitor {
      type ResultType = TList
      type Visit0 = TCons[E, TCons[H, T]]
      type VisitSucc[Pre <: Nat] = TCons[H, T#Insert[Pre, E]]
    }

    type Insert[N <: Nat, E] = N#Accept[InsertVisitor[H, T, E]]
  }

  type ::[H, T <: TList] = TCons[H, T]
  type :::[T1 <: TList, T2 <: TList] = T1#Append[T2]



  implicit def containsFn[H, T <: TList] = ContainsFn[H, H :: T]()
  implicit def containsFn2[H, H2, T <: TList](implicit fn : ContainsFn[H, T]) = ContainsFn[H, H2 :: T]()

  case class ContainsFn[H, T <: TList]()



  implicit def removeFirstFn[H, T <: TList] = RemoveFirstFn[H, H :: T, T]()
  implicit def removeFirstFn2[H, H2, T <: TList, RT <: TList](implicit fn : RemoveFirstFn[H, T, RT]) = RemoveFirstFn[H, H2 :: T, H2 :: RT]()

  case class RemoveFirstFn[H, T <: TList, RT <: TList]()

  def removeFirst[H, T <: TList, T2 <: TList](v : H, l : T)(implicit fn : RemoveFirstFn[H, T, T2]) : T2 = value[T2]



  implicit def isSubSetFn[T <: TList] = IsSubSetFn[TNil, T]()
  implicit def isSubSetFn2[H, T <: TList, T2 <: TList](implicit fn1 : ContainsFn[H, T2], fn2 : IsSubSetFn[T, T2]) = IsSubSetFn[H :: T, T2]()

  case class IsSubSetFn[H, T <: TList]()

}
