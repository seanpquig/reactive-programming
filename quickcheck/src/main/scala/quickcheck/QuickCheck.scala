package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    if(a < b)
      findMin(h2) == a
    else if(a > b)
      findMin(h2) == b
    else
      findMin(h2) == a && findMin(h2) == b
  }

  property("insertDelete") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
