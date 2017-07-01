/**
  * Created by bedux on 15.06.17.
  */
import  forcomp.Anagrams._

trait Move
case class Left()

object main {
    def main(args:Array[String]):Unit = {
      println(sentenceAnagrams(List("tea")))
      val d = Vector[Int](1,2,3,4)
     val f = Vector[Int] (2,3,4,5)
      (d foldRight 1)((a,b)=>)

    }
}
