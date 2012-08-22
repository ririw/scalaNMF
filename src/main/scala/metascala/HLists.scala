package metascala

object HLists {
  import Nats._
  import Utils._

  sealed trait HList {
    type Head
    type Tail <: HList
    type Append[L <: HList] <: HList
    type ReverseAppend[L <: HList] <: HList
    type Length <: Nat
  }

  final class HNil extends HList {
    type Head = Nothing
    type Tail = HNil
    type Append[L <: HList] = L
    type ReverseAppend[L <: HList] = L
    type Length = _0

    def ::[T](v : T) = HCons(v, this)
    def :::[L <: HList](l : L) = l
    def reverse = this
  }

  val HNil = new HNil()

  final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
    type This = HCons[H, T]
    type Head = H
    type Tail = T
    type Append[L <: HList] = HCons[H, T#Append[L]]
    type ReverseAppend[L <: HList] = Tail#ReverseAppend[HCons[H, L]]
    type Length = Succ[T#Length]
    type GetByType[N <: Nat, E] = GetByTypeFn[This, N, E]
    type ReplaceByType[N <: Nat, E] = ReplaceByTypeFn[This, N, E]

    def ::[T](v : T) = HCons(v, this)
    def :::[L <: HList](l : L)(implicit fn : AppendFn[L, This]) = fn(l, this)
    def nth[N <: Nat](implicit fn : NthFn[This, N]) : NthType[This, N] = fn(this)
    def reverse(implicit fn : ReverseAppendFn[This, HNil]) = fn(this, HNil)    
    def remove[N <: Nat](implicit fn : RemoveNthFn[This, N]) = fn(this)

    def insert[N <: Nat, E](n : N, elem : E)(implicit fn : InsertNthFn[This, N, E]) : InsertNthType[This, N, E] = insert[N, E](elem)
    def insert[N <: Nat, E](elem : E)(implicit fn : InsertNthFn[This, N, E]) : InsertNthType[This, N, E] = fn(this, elem)

    def replaceByType[N <: Nat, E](n : N, elem : E)(implicit fn : ReplaceByType[N, E]) = fn(this, elem)
    def getByType[N <: Nat, E](implicit fn : GetByType[N, E]) : E = fn(this)
  }
  
  type ::[H, T <: HList] = HCons[H, T]


  // Append
  
  implicit def hlistNilAppender[L <: HList] = AppendFn[HNil, L]((v : HNil, l : L) => l)

  implicit def hlistConsAppender[H, T <: HList, L2 <: HList, R <: HList]
    (implicit fn : AppendFn[T, L2]) = AppendFn[HCons[H, T], L2]((l1 : HCons[H, T], l2 : L2) => HCons(l1.head, fn(l1.tail, l2)))

    
  // Reverse append
    
  implicit def hlistNilReverseAppender[L <: HList] = ReverseAppendFn[HNil, L]((v : HNil, l : L) => l)

  implicit def hlistConsReverseAppender[H, T <: HList, L2 <: HList, R <: HList]
    (implicit fn : ReverseAppendFn[T, HCons[H, L2]]) = ReverseAppendFn[HCons[H, T], L2]((l1 : HCons[H, T], l2 : L2) => fn(l1.tail, HCons(l1.head, l2)))

    
  // Nth
    
  implicit def hlistConsNth0[H, T <: HList] = NthFn[HCons[H, T], _0](l => l.head)
  implicit def hlistConsNth[H, T <: HList, P <: Nat](implicit fn : NthFn[T, P]) = NthFn[HCons[H, T], Succ[P]](l => fn(l.tail))

  
  // Remove nth

  implicit def hlistRemoveNth0[H, T <: HList] = RemoveNthFn[HCons[H, T], _0](l => l.tail)
  
  implicit def hlistRemoveNth[H, T <: HList, P <: Nat](implicit fn : RemoveNthFn[T, P]) = 
    RemoveNthFn[HCons[H, T], Succ[P]](l => HCons(l.head, fn(l.tail)))


  // Insert nth

  implicit def hlistInsertNth0[L <: HList, E] = InsertNthFn[L, _0, E]((l, elem) => HCons(elem, l))
  
  implicit def hlistInsertNth[H, T <: HList, P <: Nat, E](implicit fn : InsertNthFn[T, P, E]) = 
    InsertNthFn[HCons[H, T], Succ[P], E]((l, elem) => HCons(l.head, fn(l.tail, elem)))


  // Replace by type

  implicit def hlistReplaceByType0[T <: HList, E] = ReplaceByTypeFn[HCons[E, T], _0, E]((l, elem) => HCons(elem, l.tail))
  
  implicit def hlistReplaceByTypeNthMatch[T <: HList, P <: Nat, E](implicit fn : ReplaceByTypeFn[T, P, E]) = 
    ReplaceByTypeFn[HCons[E, T], Succ[P], E]((l, elem) => HCons(elem, fn(l.tail, elem)))

  implicit def hlistReplaceByTypeNthNoMatch[H, T <: HList, N <: Nat, E](implicit fn : ReplaceByTypeFn[T, N, E]) = 
    ReplaceByTypeFn[HCons[H, T], N, E]((l, elem) => HCons(l.head, fn(l.tail, elem)))


  // Get by type

  implicit def hlistGetByType0[T <: HList, E] = GetByTypeFn[HCons[E, T], _0, E](l => l.head)
  
  implicit def hlistGetByTypeNthMatch[T <: HList, P <: Nat, E](implicit fn : GetByTypeFn[T, P, E]) = 
    GetByTypeFn[HCons[E, T], Succ[P], E](l => fn(l.tail))

  implicit def hlistGetByTypeNthNoMatch[H, T <: HList, N <: Nat, E](implicit fn : GetByTypeFn[T, N, E]) = 
    GetByTypeFn[HCons[H, T], N, E](l => fn(l.tail))


  case class AppendFn[L1 <: HList, L2 <: HList](fn : (L1, L2) => L1#Append[L2]) extends Fn2Wrapper(fn)

  case class ReverseAppendFn[L1 <: HList, L2 <: HList](fn : (L1, L2) => L1#ReverseAppend[L2]) extends Fn2Wrapper(fn)  
  
  case class NthFn[L <: HList, N <: Nat](fn : L => NthType[L, N]) extends Fn1Wrapper(fn)

  final class NthVisitor[L <: HList] extends NatVisitor {
    type ResultType = Any
    type Visit0 = L#Head
    type VisitSucc[Pre <: Nat] = Pre#Accept[NthVisitor[L#Tail]]
  }
  
  type NthType[L <: HList, N <: Nat] = N#Accept[NthVisitor[L]]

  final class RemoveNthVisitor[L <: HList] extends NatVisitor {
    type ResultType = HList
    type Visit0 = L#Tail
    type VisitSucc[Pre <: Nat] = HCons[L#Head, Pre#Accept[RemoveNthVisitor[L#Tail]]]
  }

  type RemoveNthType[L <: HList, N <: Nat] = N#Accept[RemoveNthVisitor[L]]
  
  case class RemoveNthFn[L <: HList, N <: Nat](fn : L => RemoveNthType[L, N]) extends Fn1Wrapper(fn)

  final class InsertNthVisitor[L <: HList, T] extends NatVisitor {
    type ResultType = HList
    type Visit0 = HCons[T, L]
    type VisitSucc[Pre <: Nat] = HCons[L#Head, Pre#Accept[InsertNthVisitor[L#Tail, T]]]
  }

  type InsertNthType[L <: HList, N <: Nat, E] = N#Accept[InsertNthVisitor[L, E]]
  
  case class InsertNthFn[L <: HList, N <: Nat, E](fn : (L, E) => InsertNthType[L, N, E]) extends Fn2Wrapper(fn)


  case class ReplaceByTypeFn[L <: HList, N <: Nat, E](fn : (L, E) => L) extends Fn2Wrapper(fn)

  case class GetByTypeFn[L <: HList, N <: Nat, E](fn : L => E) extends Fn1Wrapper(fn)

}
