package metascala.test

object IntsTest {
  import metascala.Integers._
  import metascala.Utils._
  import metascala.Addables._
  import metascala.Subtractables._

  type T1 = Equal[_0, _0 + _0]
  type T2 = Equal[_1, _2 - _1]
  type T3 = Equal[_1, _0 + _1]
  type T4 = Equal[_2, _1 + _1]
  type T5 = Equal[_7, _3 + _4]
  
}