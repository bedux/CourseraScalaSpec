package funsets

object Main extends App {
  import FunSets._
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val ss = map(union(s1,s2),x=>x*3)

  printSet(ss)
}
