package metascala.test

import metascala.Utils.{SubType, Equal}

object FunctionsTest {
  import metascala.Functions._

  trait F1 extends TFn1[Int, Boolean] { type Apply[T <: Int] = Boolean }
  trait F2 extends TFn1[Boolean, String] { type Apply[T <: Boolean] = String }
  type F = F1 >> F2
  type T1 = SubType[F, TFn1[Any, Any]]
  type T2 = Equal[Int -> F1, Boolean]


  trait A[T] { type B[X <: T]}
  type C[T, Z <: A[_]] = Z#B[T]
}
