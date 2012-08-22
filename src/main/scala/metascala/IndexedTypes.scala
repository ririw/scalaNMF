package metascala

object IndexedTypes {
  import Booleans._
  
  trait TypeListElem {
    type EqLengthNil <: Bool
    type EqLengthSucc[Pre <: TypeListElem] <: Bool
  }
  
  final class Nil extends TypeListElem {
    type EqLengthNil = True
    type EqLengthSucc[Pre <: TypeListElem] = False
  }
  
  trait TypeSucc[Pre <: TypeListElem] extends TypeListElem {
    type EqLengthNil = False
    type EqLengthZero[Pre <: TypeListElem] = Pre#EqLengthSucc[Pre]
    type Eq[T <: TypeListElem] = T#EqLengthSucc[TypeSucc[Pre]]
  }
}
