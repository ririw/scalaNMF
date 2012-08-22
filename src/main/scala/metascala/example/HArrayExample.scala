package metascala.example

object HArrayExample {
  import metascala.HArrays._
  import metascala.Nats._
  import metascala.Utils._

  // Create a HArray of an Int, Boolean and a pair
  val a1 = 10 :: true :: (10.1, "Hello") :: HArrayNil

  // Extract the second element, note that the element type
  // information is preserved and we can safely perform a
  // boolean and operation
  val b = a1(_1) && false

  // Create another HArray using alternative syntax (faster)
  val a2 = HArray(1.1, "string", false)

  // Replace the second element in the list, it used to
  // be a String, but now it's an Int
  val a3 = a2.removeNth(_1).insert(_1, 14)

  // Type information preserved, we can use an Int operation
  // on the element
  val i = a3(_1) / 2

  // Append l2 to l1
  val a4 = a1 ::: a2

  // Statically check that the length of l4 is 6
  type T = Equal[_6, a4.Size]
}
