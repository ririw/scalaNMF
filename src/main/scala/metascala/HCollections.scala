package metascala

object HCollections {
  import Nats._
  import TLists._

  trait HCollection {
    type Size <: Nat
  }

  trait HSeq[L <: TList] extends HCollection {
    type Types = L
    type Size = Types#Length
    type This[L <: TList] <: HSeq[L]

    type INth[N <: Nat]
    type IRemoveNth[N <: Nat]
    type IInsert[N <: Nat, E]

    def ::[T](v : T) : This[T :: L]
    def :::[L2 <: TList](l : This[L2]) : This[L2#Append[L]]
    def reverse : This[L#Reverse]

    def apply[N <: Nat](implicit fn : INth[N]) : L#Nth[N]
    def apply[N <: Nat](n : N)(implicit fn : INth[N]) : L#Nth[N] = apply[N]

    def removeNth[N <: Nat](implicit fn : IRemoveNth[N]) : This[L#RemoveNth[N]]
    def removeNth[N <: Nat](n : N)(implicit fn : IRemoveNth[N]) : This[L#RemoveNth[N]] = removeNth[N]

    def insert[N <: Nat, E](elem : E)(implicit fn : IInsert[N, E]) : This[L#Insert[N, E]]
    def insert[N <: Nat, E](n : N, elem : E)(implicit fn : IInsert[N, E]) : This[L#Insert[N, E]] = insert[N, E](elem)

    //    def replaceSameType[N <: Nat, E](n : N, elem : E) : This[L]
  }

}
