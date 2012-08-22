package metascala.test

import metascala.Utils.Equal

object HArrayTest {
  import metascala.TLists._
  import metascala.Nats._
  import metascala.HArrays._

  // Cons
  val l1 : HArray[Int :: Boolean :: TNil] = 10 :: true :: HArrayNil
  val l2 : HArray[Double :: String :: TNil] = 10.1 :: "hello" :: HArrayNil
  val l3 : HArray[(Int, Boolean) :: Int :: Boolean :: TNil] = (10, false) :: l1

  object Append {
    val a1 : HArray[Int :: TNil] = (10 :: HArrayNil) ::: HArrayNil
    val a2 : HArray[Int :: TNil] = HArrayNil ::: (10 :: HArrayNil)
    val a3 : HArray[Boolean :: Int :: TNil] = (true :: HArrayNil) ::: (10 :: HArrayNil)
    val a4 : HArray[(Int, Boolean) :: Int :: Boolean :: Double :: String :: Int :: Boolean :: TNil] = l3 ::: l2 ::: l1
  }

  object Nth {
    val n1 = l1(_0) * 3
    val n2 = l1(_1) && false
    val n3 = l3(_2) && true
    val n4 = l2(_1).length
    val n5 = l3(_0)._1 * 3
  }

  object Reverse {
    val r1 = HArrayNil.reverse
    val r2 : HArray[Int :: TNil] = (10 :: HArrayNil).reverse
    val r3 : HArray[Boolean :: Int :: TNil] = l1.reverse
    val r4 : HArray[Boolean :: Int :: (Int, Boolean) :: TNil] = l3.reverse
  }

  object Length {
    type L1 = Equal[l1.Size, _2]
    type L2 = Equal[l3.Size, _3]
  }

  object Remove {
    val r1 : HArray[Boolean :: TNil] = l1.removeNth(_0)
    val r2 : HArray[Int :: TNil] = l1.removeNth(_1)
    val r3 : HArray[TNil] = r1.removeNth(_0)
    val r4 : HArray[(Int, Boolean) :: Int :: TNil] = l3.removeNth(_2)
  }

  object Insert {
    val i1 : HArray[String :: Int :: Boolean :: TNil] = l1.insert(_0, "Hello")
    val i2 : HArray[Int :: String :: Boolean :: TNil] = l1.insert(_1, "Hello")
    val i3 : HArray[Int :: Boolean :: String :: TNil] = l1.insert(_2, "Hello")
    val i4 : HArray[Int :: String :: TNil] = l1.removeNth(_1).insert(_1, "Hello")
  }

  object GetByType {
//    val g1 = l1.getByType[_0, Int]
//    val g2 = l1.getByType[_0, Boolean]
  }

  object ReplaceByType {
//    val r1 = l1.replaceByType(_0, 1)
//    val r2 = l1.replaceByType(_0, false)
  }

  val t : HArray[Int :: Boolean :: String :: TNil] = HArray(10, true, "Hello")
  val t2 : HArray[Double :: Int :: Boolean :: String :: TNil] = 10.1 :: t
  val t3 : HArray[Double :: Int :: Boolean :: String :: Int :: Boolean :: String :: TNil] = t2 ::: t
  val t4 : HArray[String :: Int :: Boolean :: String :: TNil] = "x" :: t
  val t5 : HArray[Int :: TNil] = HArray(10)
  val t6 : HArray[String :: TNil] = HArray("a")
  val t7 : HArray[Int :: String :: TNil] = t5 ::: t6
  val t8 : Boolean = t(_1)
  val t9 : HArray[String :: Boolean :: Int :: TNil] = t.reverse
  val t9b : HArray[String :: Boolean :: Boolean :: Int :: TNil] = t9.insert(_2, false)
  val t10 : HArray[Double :: Int :: Boolean :: String :: Boolean :: String :: TNil] = t3.removeNth(_4)
  val t11 : HArray[Double :: Int :: Boolean :: Boolean :: String :: Boolean :: String :: TNil] = t10.insert(_3, false)
}
