package metascala.example

object HListExample {
  import metascala.HLists._
  import metascala.Nats._
  import metascala.Utils._
  
  // Create a HList of an Int, Boolean and a pair
  val l1 = 10 :: true :: (10.1, "Hello") :: HNil
  
  // Extract the second element, note that the element type 
  // information is preserved and we can safely perform a 
  // boolean and operation
  l1.nth[_1] && false
  
  // Create another HList, note the use of an operator in 
  // the type expression 
  val l2 : Double :: String :: Boolean :: HNil = 1.1 :: "string" :: false :: HNil
  
  // Replace the second element in the list, it used to
  // be a String, but now it's an Int
  val l3 = l2.remove[_1].insert(_1, 14)
  
  // Type information preserved, we can use an Int operation
  // on the element
  l3.nth[_1] * 10

  // Append l2 to l1
  val l4 = l1 ::: l2

  // Statically check that the length of l4 is 6
  type T = Equal[_6, l4.Length]
}
