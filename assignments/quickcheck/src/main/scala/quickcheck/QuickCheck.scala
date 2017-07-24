package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin after insert min into heap gives min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMind after two elements ins into empty heap is smallest") = forAll { (i: Int, j: Int) =>
    val firstInserted = insert(i, empty)
    val secondInserted = insert(j, firstInserted)

    Math.min(i, j) == findMin(secondInserted)
  }

  property("Deleting minumum of 1 element heap is empty heap") = forAll { (i: Int) =>
    val h = insert(i, empty)
    isEmpty(deleteMin(h))
  }

  property("findMin/deleteMin/findMind etc. of arbitrary heap is ordered") = forAll { (h: H) =>
    def isOrderedRecur(h: H, prevMin: Int): Boolean = {
      if (isEmpty(h)) true
      else if (findMin(h) < prevMin) false
      else isOrderedRecur(deleteMin(h), findMin(h))
    }
    isOrderedRecur(h, Int.MinValue)
  }

  property("findMin of two melded arbitrary heaps is min of both heaps individually") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2)) {
      val (min1, min2) = (findMin(h1), findMin(h2))
      Math.min(min1, min2) == findMin(meld(h1, h2))
    }
    else true
  }

}
