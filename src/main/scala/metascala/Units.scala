package metascala

object Units {
  import Integers._
  import Utils._
  import Addables._
  import Subtractables._

  trait Unit {
    type M <: MInt
    type KG <: MInt
    type S <: MInt
    type A <: MInt
    type K <: MInt
    type Mol <: MInt
    type CD <: MInt
  }
  
  final class TUnit[_M <: MInt, _KG <: MInt, _S <: MInt, _A <: MInt, _K <: MInt, _Mol <: MInt, _CD <: MInt] {
    type M = _M
    type KG = _KG
    type S = _S
    type A = _A
    type K = _K
    type Mol = _Mol
    type CD = _CD
  }
  
  case class Quantity[M <: MInt, KG <: MInt, S <: MInt, A <: MInt, K <: MInt, Mol <: MInt, CD <: MInt](value : Double) {
    type This = Quantity[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value + m.value)
    def -(m : This) = Quantity[M, KG, S, A, K, Mol, CD](value - m.value)
    
    def *[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](
        m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = 
          Quantity[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    
    def /[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](
        m : Quantity[M2, KG2, S2, A2, K2, Mol2, CD2]) = 
          Quantity[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value / m.value)
    
    def apply(v : Double) = Quantity[M, KG, S, A, K, Mol, CD](v * value)
  }
  
  implicit def measure(v : Double) = Quantity[_0, _0, _0, _0, _0, _0, _0](v)
  
  val m = Quantity[_1, _0, _0, _0, _0, _0, _0](1)
  val kg = Quantity[_0, _1, _0, _0, _0, _0, _0](1)
  val s = Quantity[_0, _0, _1, _0, _0, _0, _0](1)
  val a = Quantity[_0, _0, _0, _1, _0, _0, _0](1)
  val k = Quantity[_0, _0, _0, _0, _1, _0, _0](1)
  val mol = Quantity[_0, _0, _0, _0, _0, _1, _0](1)
  val cd = Quantity[_0, _0, _0, _0, _0, _0, _1](1)
  
  type Length = Quantity[_1, _0, _0, _0, _0, _0, _0]
  type Mass = Quantity[_0, _1, _0, _0, _0, _0, _0]
  type Time = Quantity[_0, _0, _1, _0, _0, _0, _0]
  type Currency = Quantity[_0, _0, _0, _1, _0, _0, _0]
  type Temperature = Quantity[_0, _0, _0, _0, _1, _0, _0]
  type Mol = Quantity[_0, _0, _0, _0, _0, _1, _0]
  type LuminousIntensity = Quantity[_0, _0, _0, _0, _0, _0, _1]
  
  type Area = Quantity[_2, _0, _0, _0, _0, _0, _0] 
  type Volume = Quantity[_3, _0, _0, _0, _0, _0, _0] 
  type Speed = Quantity[_1, _0, _1#Neg, _0, _0, _0, _0] 
  type Acceleration = Quantity[_1, _0, _2#Neg, _0, _0, _0, _0] 
}
