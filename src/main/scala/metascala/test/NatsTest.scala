package metascala.test

object NatsTest {
  import metascala.Nats._
  import metascala.Utils._
  import metascala.Addables._

  type T1 = Equal[_0, _0 + _0]
  type T2 = Equal[_1, _1 + _0]
  type T3 = Equal[_1, _0 + _1]
  type T4 = Equal[_2, _1 + _1]
  type T5 = Equal[_7, _3 + _4]

  /*trait Fib extends NatVisitor {
    type ResultType = Nat
    type Visit0 = _0
    type VisitSucc[Pre <: Nat] = Pre#Is0#If2[Nat, _1, Pre#Accept[Fib] + Pre#Pre#Accept[Fib]]
  }

  type F0 = Equal[_0, _0#Accept[Fib]]
  type F1 = Equal[_1, _1#Accept[Fib]]
  type F2 = Equal[_1, _2#Accept[Fib]]
  type F3 = Equal[_2, _3#Accept[Fib]]
  type F4 = Equal[_3, _4#Accept[Fib]]
  type F5 = Equal[_5, _5#Accept[Fib]]*/

}
