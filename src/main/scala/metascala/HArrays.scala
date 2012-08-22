package metascala


object HArrays {
  import Nats._
  import TLists._
  import HCollections._
  import Booleans._
  import Comparables._

  final case class HArrayInsertNth[L <: TList, N <: Nat](index : Int)

  implicit def insertNth0[L <: TList] = HArrayInsertNth[L, _0](0)
  implicit def insertNth[H, T <: TList, P <: Nat](implicit n : HArrayInsertNth[T, P]) = HArrayInsertNth[H :: T, Succ[P]](n.index + 1)

  final case class HArrayNth[L <: TList, N <: Nat](index : Int)

  implicit def nth0[H, T <: TList] = HArrayNth[H :: T, _0](0)
  implicit def nth[H, T <: TList, P <: Nat](implicit n : HArrayNth[T, P]) = HArrayNth[H :: T, Succ[P]](n.index + 1)

  final class HArray[L <: TList](private val elems : Array[Any]) extends HSeq[L] {
    type This[L <: TList] = HArray[L]

    type INth[N <: Nat] = HArrayNth[L, N]
    type IRemoveNth[N <: Nat] = INth[N]
    type IInsert[N <: Nat, E] = HArrayInsertNth[L, N]

    def ::[T](v : T) = {
      val a = new Array[Any](elems.length + 1)
      a(0) = v
      Array.copy(elems, 0, a, 1, elems.length)
      new HArray[T :: L](a)
    }

    def :::[L2 <: TList](l : HArray[L2]) = {
      val a = new Array[Any](elems.length + l.elems.length)
      Array.copy(l.elems, 0, a, 0, l.elems.length)
      Array.copy(elems, 0, a, l.elems.length, elems.length)
      new HArray[L2#Append[L]](a)
    }

    def apply[N <: Nat](implicit nth : INth[N]) : L#Nth[N] = elems(nth.index).asInstanceOf[L#Nth[N]]

    def reverse = {
      val a = new Array[Any](elems.length)

      for (i <- 0 until elems.length)
        a(i) = elems(elems.length - 1 - i)

      new HArray[L#ReverseAppend[TNil]](a)
    }

    def removeNth[N <: Nat](implicit nth : INth[N]) = {
      val a = new Array[Any](elems.length - 1)
      val i = nth.index
      Array.copy(elems, 0, a, 0, i)
      Array.copy(elems, i + 1, a, i, elems.length - 1 - i)
      new HArray[L#RemoveNth[N]](a)
    }

    def insert[N <: Nat, E](elem : E)(implicit nth : HArrayInsertNth[L, N]) = {
      val a = new Array[Any](elems.length + 1)
      val i = nth.index
      Array.copy(elems, 0, a, 0, i)
      a(i) = elem
      Array.copy(elems, i, a, i + 1, elems.length - i)
      new HArray[L#Insert[N, E]](a)
    }

    override def equals(o : Any) = o match {
      case ha : HArray[_] => HArray.equal(elems, ha.elems)
      case _ => false
    }

    override def hashCode = HArray.hashCode(elems)

    override def toString = "HArray(" + elems.mkString(", ") + ")"
//    def replaceSameType[N <: Nat, E](n : N, elem : E) = null
//    def getByType[N <: Nat, E](implicit fn : GetByType[N, E]) : E = fn(this)
  }

  val HArrayNil = new HArray[TNil](new Array[Any](0))

  object HArray {
    private def createArray(elems : Any*) = elems toArray

    private def equal[T](a1 : Array[T], a2 : Array[T]) : Boolean = {
      if (a1.length != a2.length)
        return false
      else {
        var i = 0

        while (i < a1.length) {
          if (a1(i) != a2(i))
            return false

          i += 1
        }

        return true
      }
    }

    private def hashCode[T](a : Array[T]) : Int = {
      var i = 0
      var h = 0

      while (i < a.length) {
        h = 37 * h + a(i).hashCode
        i += 1
      }

      return h
    }

    def apply[T1](v1 : T1) : HArray[T1 :: TNil] =
      new HArray[T1 :: TNil](createArray(v1))

    def apply[T1, T2](v1 : T1, v2 : T2) : HArray[T1 :: T2 :: TNil] =
      new HArray[T1 :: T2 :: TNil](createArray(v1, v2))

    def apply[T1, T2, T3](v1 : T1, v2 : T2, v3 : T3) : HArray[T1 :: T2 :: T3 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: TNil](createArray(v1, v2, v3))

    def apply[T1, T2, T3, T4](v1 : T1, v2 : T2, v3 : T3, v4 : T4) : HArray[T1 :: T2 :: T3 :: T4 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: T4 :: TNil](createArray(v1, v2, v3, v4))

    def apply[T1, T2, T3, T4, T5](v1 : T1, v2 : T2, v3 : T3, v4 : T4, v5 : T5) : HArray[T1 :: T2 :: T3 :: T4 :: T5 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: T4 :: T5 :: TNil](createArray(v1, v2, v3, v4, v5))
  }
}
