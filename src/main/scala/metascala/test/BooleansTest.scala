package metascala.test

object BooleansTest {
  import metascala.Booleans._
  import metascala.Utils._

  object And {
    type T1 = Equal[False, False && False]
    type T2 = Equal[False, True && False]
    type T3 = Equal[False, False && True]
    type T4 = Equal[True, True && True]
  }

  object Or {
    type T1 = Equal[False, False || False]
    type T2 = Equal[True, True || False]
    type T3 = Equal[True, False || True]
    type T4 = Equal[True, True || True]
  }
  
  object Not {
    type T1 = Equal[True, False#Not]
    type T2 = Equal[False, True#Not]    
  }

  object If {
    type T1 = Equal[Int, True#If[Int, Boolean]]
    type T2 = Equal[Boolean, False#If[Int, Boolean]]    
  }
}
