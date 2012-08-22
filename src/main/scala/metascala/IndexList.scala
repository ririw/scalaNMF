package metascala

import metascala.Nats._
import metascala.Utils._

object IndexList {
  sealed trait IndexList[Type] {
    type Head
    type Tail <: IndexList[Type]
    type Length <: Nat
    def toList : List[Type]
  }
  final class IndexListNil[Type] extends IndexList[Type] {
    type Head = Nothing
    type Tail = IndexListNil[Type]
    type Length = _0

    def ::(v : Type) = IndexListCons(v, this)
    def toList : List[Type] = List[Type]()

  }
  final case class IndexListCons[Type, T <: IndexList[Type]](head : Type, tail : T)
    extends IndexList[Type] {
    type Head = Type
    type Tail = T
    type Length = Succ[T#Length]

    def ::(v : Type) = IndexListCons(v, this)
    def toList : List[Type] = head :: (tail.toList)
  }
  type ::[Type, T <: IndexList[Type]] = IndexListCons[Type, T]

  sealed trait Vec[T, N <: Nat] {
    def toList : List[T]
    type AsVec
    type Next <: Vec[T, Succ[N]]
  }
  final case class VecOne[T](v : T) extends Vec[T, _1] {
    def toList : List[T] = List(v)
    def ::(v : T) = VecCons(v, this)
    type AsVec <: Vec[T, _1]
  }
  final case class VecCons[T, N <: Nat](head : T, tail : Vec[T, N]) extends Vec[T, Succ[N]] {
    def toList : List[T] = head :: tail.toList
    def ::(v : T) : VecCons[T, Succ[N]] = VecCons(v, this)
    type AsVec <: Vec[T, N]
  }

  val x = 1.0 :: 2.0 :: (new IndexListNil())
  val y = 1.0 :: (new IndexListNil())
  val z = new IndexListNil[Double]

  val vz : VecOne[Int] = VecOne(1)
  val vy : VecCons[Int, _1] = VecCons(2, vz)
  val vx : VecCons[Int, _2] = VecCons(3, vy)

  def innerEqualI[A <: IndexList[Double], B <: IndexList[Double]](a : A, b : B)(implicit evidence : A#Length =:= B#Length)
  : Boolean = {
    a.toList == b.toList
  }
  def innerEqualV[T, N <: Nat](a : VecCons[T, N], b : VecCons[T, N]) : Boolean = {
    a.toList == b.toList
  }
  def innerEqualV[T](a : VecOne[T], b : VecOne[T]) : Boolean = {
    a.toList == b.toList
  }
  val works = innerEqualI(x, x) && innerEqualV(vx, vx) && innerEqualV(vz, vz)
  /*
  def listToVec[T, N <: Nat](list : List[T]) : Vec[T, N] = list match {
    case Nil => throw new Exception
    case (l :: Nil) => lastVal(l)
    case (l :: ls)  => VecCons(l, listToVec(ls))
  }
  */
  // The scala compiler cannot infer that the if-check will
  // ensure that vecOne is of type Vec[T,N]
  def lastVal[T, N <: Nat](v : T) : Vec[T, N] = {
    if (value[N].toInt == 1) {
      VecOne(v).asInstanceOf[Vec[T,N]]
    }else{
      throw new Exception
    }
  }
  def anotherTest(c: List[Int]) = {
    val a = List(1, 2, 3)
    val b = List(4, 6, 7)
    val av : Vec[Int, _3] = a match {
      case Nil => throw new Exception
      case x :: xs => x :: vy
    }
  }
}
