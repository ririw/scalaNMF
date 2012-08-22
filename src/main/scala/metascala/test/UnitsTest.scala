package metascala.test

object UnitsTest {
  import metascala.Units._
  import metascala.Subtractables._
  
  val dist : Length = m(2.3)
  val time : Time = s(1.7)
  val x = dist * time
  val speed : Speed = dist / time
}
