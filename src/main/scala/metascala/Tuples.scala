package metascala

object Tuples {
  import Utils._
  
  case class RichProduct[P <: Product](p : P) {
    def get[T](implicit getter : Getter[P, T]) = getter(p)
    def replace[T](v : T)(implicit replacer : Replacer[P, T]) = replacer(p, v)
  }
  
  implicit def productToRichProduct[P <: Product](p : P) = RichProduct(p)
  
  implicit def append[T1, T2](t1 : T1, t2 : T2) = (t1, t2)
  implicit def append[T1, T2, T3](t1 : T1, t2 : (T2, T3)) = (t1, t2._1, t2._2)
  implicit def append[T1, T2, T3, T4](t1 : T1, t2 : (T2, T3, T4)) = (t1, t2._1, t2._2, t2._3)
  implicit def append[T1, T2, T3, T4, T5](t1 : T1, t2 : (T2, T3, T4, T5)) = (t1, t2._1, t2._2, t2._3, t2._4)
  
  implicit def append[T1, T2, T3](t1 : (T1, T2), t2 : T3) = (t1._1, t1._2, t2)
  implicit def append[T1, T2, T3, T4](t1 : (T1, T2), t2 : (T3, T4)) = (t1._1, t1._2, t2._1, t2._2)
  implicit def append[T1, T2, T3, T4, T5](t1 : (T1, T2), t2 : (T3, T4, T5)) = (t1._1, t1._2, t2._1, t2._2, t2._3)
  implicit def append[T1, T2, T3, T4, T5, T6](t1 : (T1, T2), t2 : (T3, T4, T5, T6)) = (t1._1, t1._2, t2._1, t2._2, t2._3, t2._4)

  implicit def append[T1, T2, T3, T4](t1 : (T1, T2, T3), t2 : T4) = (t1._1, t1._2, t1._3, t2)
  implicit def append[T1, T2, T3, T4, T5](t1 : (T1, T2, T3), t2 : (T4, T5)) = (t1._1, t1._2, t1._3, t2._1, t2._2)
  implicit def append[T1, T2, T3, T4, T5, T6](t1 : (T1, T2, T3), t2 : (T4, T5, T6)) = (t1._1, t1._2, t1._3, t2._1, t2._2, t2._3)
  implicit def append[T1, T2, T3, T4, T5, T6, T7](t1 : (T1, T2, T3), t2 : (T4, T5, T6, T7)) = (t1._1, t1._2, t1._3, t2._1, t2._2, t2._3, t2._4)
  
  implicit def append[T1, T2, T3, T4, T5](t1 : (T1, T2, T3, T4), t2 : T5) = (t1._1, t1._2, t1._3, t1._4, t2)
  implicit def append[T1, T2, T3, T4, T5, T6](t1 : (T1, T2, T3, T4), t2 : (T5, T6)) = (t1._1, t1._2, t1._3, t1._4, t2._1, t2._2)
  implicit def append[T1, T2, T3, T4, T5, T6, T7](t1 : (T1, T2, T3, T4), t2 : (T5, T6, T7)) = (t1._1, t1._2, t1._3, t1._4, t2._1, t2._2, t2._3)
  implicit def append[T1, T2, T3, T4, T5, T6, T7, T8](t1 : (T1, T2, T3, T4), t2 : (T5, T6, T7, T8)) = (t1._1, t1._2, t1._3, t1._4, t2._1, t2._2, t2._3, t2._4)
  

  def replace[P, T](t : P, v : T)(implicit replacer : Replacer[P, T]) = replacer(t, v)
  
  case class Replacer[P, T](fn : (P, T) => P) extends Fn2Wrapper(fn)
  
  implicit def replace11[T] = Replacer[T, T]((t, v) => v)
  
  implicit def replace21[T1, T2] = Replacer[(T1, T2), T1]((t, v) => (v, t._2))
  implicit def replace22[T1, T2] = Replacer[(T1, T2), T2]((t, v) => (t._1, v))
  
  implicit def replace31[T1, T2, T3] = Replacer[(T1, T2, T3), T1]((t, v) => (v, t._2, t._3))
  implicit def replace32[T1, T2, T3] = Replacer[(T1, T2, T3), T2]((t, v) => (t._1, v, t._3))
  implicit def replace33[T1, T2, T3] = Replacer[(T1, T2, T3), T3]((t, v) => (t._1, t._2, v))

  implicit def replace41[T1, T2, T3, T4] = Replacer[(T1, T2, T3, T4), T1]((t, v) => (v, t._2, t._3, t._4))
  implicit def replace42[T1, T2, T3, T4] = Replacer[(T1, T2, T3, T4), T2]((t, v) => (t._1, v, t._3, t._4))
  implicit def replace43[T1, T2, T3, T4] = Replacer[(T1, T2, T3, T4), T3]((t, v) => (t._1, t._2, v, t._4))
  implicit def replace44[T1, T2, T3, T4] = Replacer[(T1, T2, T3, T4), T4]((t, v) => (t._1, t._2, t._3, v))
  
  implicit def reverse[T1, T2](t : (T1, T2)) = (t._2, t._1)
  implicit def reverse[T1, T2, T3](t : (T1, T2, T3)) = (t._3, t._2, t._1)
  implicit def reverse[T1, T2, T3, T4](t : (T1, T2, T3, T4)) = (t._4, t._3, t._2, t._1)
  implicit def reverse[T1, T2, T3, T4, T5](t : (T1, T2, T3, T4, T5)) = (t._5, t._4, t._3, t._2, t._1)
  implicit def reverse[T1, T2, T3, T4, T5, T6](t : (T1, T2, T3, T4, T5, T6)) = (t._6, t._5, t._4, t._3, t._2, t._1)

  def get[P, T](p : P)(implicit getter : Getter[P, T]) = getter(p)
  
  case class Getter[T, VT](fn : T => VT) extends Fn1Wrapper(fn)
  
  implicit def get11[T] = Getter[T, T](v => v)

  implicit def get21[T1, T2] = Getter[(T1, T2), T1](t => t._1)
  implicit def get22[T1, T2] = Getter[(T1, T2), T2](t => t._2)

  implicit def get31[T1, T2, T3] = Getter[(T1, T2, T3), T1](t => t._1)
  implicit def get32[T1, T2, T3] = Getter[(T1, T2, T3), T2](t => t._2)
  implicit def get33[T1, T2, T3] = Getter[(T1, T2, T3), T3](t => t._3)

  implicit def get41[T1, T2, T3, T4] = Getter[(T1, T2, T3, T4), T1](t => t._1)
  implicit def get42[T1, T2, T3, T4] = Getter[(T1, T2, T3, T4), T2](t => t._2)
  implicit def get43[T1, T2, T3, T4] = Getter[(T1, T2, T3, T4), T3](t => t._3)
  implicit def get44[T1, T2, T3, T4] = Getter[(T1, T2, T3, T4), T4](t => t._4)

  implicit def get51[T1, T2, T3, T4, T5] = Getter[(T1, T2, T3, T4, T5), T1](t => t._1)
  implicit def get52[T1, T2, T3, T4, T5] = Getter[(T1, T2, T3, T4, T5), T2](t => t._2)
  implicit def get53[T1, T2, T3, T4, T5] = Getter[(T1, T2, T3, T4, T5), T3](t => t._3)
  implicit def get54[T1, T2, T3, T4, T5] = Getter[(T1, T2, T3, T4, T5), T4](t => t._4)
  implicit def get55[T1, T2, T3, T4, T5] = Getter[(T1, T2, T3, T4, T5), T5](t => t._5)
}
