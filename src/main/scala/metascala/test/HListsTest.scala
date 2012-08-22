package metascala.test

object HListTest {
  import metascala.Nats._
  import metascala.HLists._
  import metascala.Utils._
  
  // Cons
  val l1 : Int :: Boolean :: HNil = 10 :: true :: HNil
  val l2 : Double :: String :: HNil = 10.1 :: "hello" :: HNil
  val l3 : (Int, Boolean) :: Int :: Boolean :: HNil = (10, false) :: l1

  object Append {
    val a1 : Int :: HNil = (10 :: HNil) ::: HNil
    val a2 : Int :: HNil = HNil ::: (10 :: HNil)
    val a3 : Boolean :: Int :: HNil = (true :: HNil) ::: (10 :: HNil)
    val a4 : (Int, Boolean) :: Int :: Boolean :: Double :: String :: Int :: Boolean :: HNil = l3 ::: l2 ::: l1
  }
  
  object Nth {
    val n1 = l1.nth[_0] * 3
    val n2 = l1.nth[_1] && false
    val n3 = l3.nth[_2] && true
    val n4 = l2.nth[_1].length
    val n5 = l3.nth[_0]._1 * 3
  }
  
  object Reverse {
    val r1 = HNil.reverse
    val r2 : Int :: HNil = (10 :: HNil).reverse
    val r3 : Boolean :: Int :: HNil = l1.reverse
    val r4 : Boolean :: Int :: (Int, Boolean) :: HNil = l3.reverse
  }

  object Length {
    type L1 = Equal[l1.type#Length, _2]
    type L2 = Equal[l3.type#Length, _3]
  }
  
  object Remove {
    val r1 : Boolean :: HNil = l1.remove[_0]
    val r2 : Int :: HNil = l1.remove[_1]
    val r3 : HNil = l1.remove[_0].remove[_0]
    val r4 : (Int, Boolean) :: Int :: HNil = l3.remove[_2]
  }

  object Insert {
    val i1 : String :: Int :: Boolean :: HNil = l1.insert(_0, "Hello")
    val i2 : Int :: String :: Boolean :: HNil = l1.insert(_1, "Hello")
    val i3 : Int :: Boolean :: String :: HNil = l1.insert(_2, "Hello")
    val i4 : Int :: String :: HNil = l1.remove[_1].insert(_1, "Hello")
  }
  
  object GetByType {
    val g1 = l1.getByType[_0, Int]
    val g2 = l1.getByType[_0, Boolean]
  }
  
  object ReplaceByType {
    val r1 = l1.replaceByType(_0, 1)
    val r2 = l1.replaceByType(_0, false)
  }
  
}
