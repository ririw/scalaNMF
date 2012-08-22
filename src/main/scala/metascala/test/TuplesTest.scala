package metascala.test

object TuplesTest {
  import metascala.Tuples._
  
  val t1 = (10, "Hello", true)
  val t2 = ((10, 20), 10.1)
  val t3 : (Int, String, Boolean, (Int, Int), Double) = append(t1, t2)
  val t4 : (Double, (Int, Int), Boolean, String, Int) = reverse(t3)
  val t5 : (Int, String, Boolean) = t1 replace 5 replace false replace "new string"
//  val t6 = (10, 20) replace 5
//  val t7 : (Boolean, Int, String, Boolean) = append(true, t1)
  val t8 = t1.get[Int]
}
