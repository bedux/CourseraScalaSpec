package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  def ToList(h:H):List[A] =
  {
    if (isEmpty(h))
      List()
    else{
      findMin(h):: ToList(deleteMin(h))
    }
  }


  lazy val genHeap: Gen[H] = for{
    x <- arbitrary[A]
    h <- oneOf(genHeap,const(empty))

  }yield insert(x,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("Is Not Empty") = forAll{(a:Int) =>
    val h = insert(1,empty)
    !isEmpty(h)
  }

  property("isEmpty") = forAll{(a:Int) => {
    val h = insert(a,empty);
    isEmpty(deleteMin(h))
  }}

  property("isMin") = forAll{ (h:H)=>
    if(isEmpty (h))  true
    else {
      val min = findMin(h)
      val o = deleteMin(h)
      if (!isEmpty(o)) {
        val nM = findMin(o)
        nM >= min
      }
      else
        true
    }
  }
  property("merge")= forAll{(a:Int,b:Int) =>
    val c1 = insert(a,empty)
    val c2 = insert(b,empty)
    val c3 = meld(c1,c2)
    if(a < b) {
      findMin(c3) == a
    }else
    findMin(c3) == b
  }
  property("merge empty") = forAll{(a:Int) =>
    val c1 = meld(empty,insert(a,empty))
    findMin(c1) == a
  }

  property("delete min") = forAll{(a:Int,b:Int) =>
    val c1 = insert(a,insert(b,empty))
    val o = findMin(deleteMin(c1))
    o == (if (a > b) a else b)
  }
  val smallInteger = Gen.choose(0,100)
  property("delete min ") = forAll(smallInteger){(a:Int ) =>

          val c1 =insert(a+8,deleteMin( (insert(a, insert(a+1,insert(a+2,empty))))))
          ToList(c1).contains(a+1) && ToList(c1).contains(a+2) && ToList(c1).contains(a+8)






  }
  property("multiple merg") = forAll{(h:H,h1:H,h2:H)=>
    val m = findMin(meld(meld(h,h1),h2))
    m == Math.min(findMin(h),Math.min(findMin(h1),findMin(h2)))
  }



}
