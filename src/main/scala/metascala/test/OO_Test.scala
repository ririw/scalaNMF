package metascala.test

  object OO_Tuples {
    import metascala.OO._
    import metascala.Tuples._

/* No longer compiles with 2.8

    type Point = (Print, X, Y, Add)

    case class Print(fn : Point => Unit) extends Method0(fn)
    case class X(fn : Point => Int) extends Method0(fn)
    case class Y(fn : Point => Int) extends Method0(fn)
    case class Add(fn : (Point, Point) => Point) extends Method1(fn)
  
    def point(x : Int, y : Int) : Point = (
      Print((p : Point) => println(p.call[X] + ", " + p.call[Y])), 
      X(p => x),
      Y(p => y), 
      Add((p1, p2) => point(p1.call[X] + p2.call[X], p1.call[Y] + p2.call[Y]))
    )
  
    def overridePoint(dp : Point) : Point = dp |= 
      Print(p => {println("Before print"); dp.delegate[Print](p); println("After print")}) |=
      X(p => dp.delegate[X](p) + 10)

    def main(args : Array[String]) {
      val p1 = point(10, 20)    
      val p2 = overridePoint(p1)
      val p3 : Point = p1.call[Add](p2)
      p1.call[Print]
      p2.call[Print]
      p3.call[Print]
    }*/
  }

/*  object HList {
    import OO._
    import HLists._
  
    type Point = Print :: X :: Y :: Add :: HNil
  
    case class Print(fn : Point => Unit) extends Method0(fn)
    case class X(fn : Point => Int) extends Method0(fn)
    case class Y(fn : Point => Int) extends Method0(fn)
    case class Add(fn : (Point, Point) => Point) extends Method1(fn)
  
    def point(x : Int, y : Int) : Point =
      Print(p => println(p.call[X] + ", " + p.call[Y])) :: 
      X(p => x) ::
      Y(p => y) :: 
      Add((p1, p2) => point(p1.call[X] + p2.call[X], p1.call[Y] + p2.call[Y])) ::
      HNil
  
    def overridePoint(dp : Point) : Point = dp |= 
      Print(p => {println("Before print"); dp.delegate[Print](p); println("After print")}) |=
      X(p => dp.delegate[X](p) + 10)

    def main(args : Array[String]) {
      val p1 = point(10, 20)    
      val p2 = overridePoint(p1)
      val p3 = p1.call[Add](p2)
      p1.call[Print]
      p2.call[Print]
      p3.call[Print]
    }
  }*/
