package main
import scalaz._
import Scalaz._

trait MyMonad[A]{
  def zero:A
  def mapped(f1: A, f2: A): A;
}


 object IntMonoid extends MyMonad[Int] {
  def zero: Int = 0
  def mapped(f1: Int, f2: Int): Int = f1 + f2

}


object StringMonoid extends MyMonad[String]{
  def zero: String = ""
  def mapped(f1: String, f2: String): String = f1 + f2

}

 object Monoidian{
  implicit val intMonoid = IntMonoid
  implicit  val stringMonoid = StringMonoid
}

 object Monoidian1{
  implicit val intMonoid = new MyMonad[Int] {
    def zero: Int = 1
    def mapped(f1: Int, f2: Int): Int = f1 * f2
  }
  implicit  val stringMonoid = StringMonoid
}

object Main{

  def main(str:Array[String]):Unit = {
  }


}
