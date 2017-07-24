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

  def isOrderedRecur(h: H, prevMin: Int): Boolean = {
    if (isEmpty(h)) true
    else if (findMin(h) < prevMin) false
    else isOrderedRecur(deleteMin(h), findMin(h))
  }

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

  property("isEmpty of 1 element heap is false") = forAll { (i: Int) =>
    val h = insert(i, empty)
    !isEmpty(h)
  }

  property("findMin/deleteMin/findMind etc. of arbitrary heap is ordered") = forAll { (h: H) =>
    isOrderedRecur(h, Int.MinValue)
  }

  property("findMin of two melded arbitrary heaps is min of both heaps individually") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2)) {
      val (min1, min2) = (findMin(h1), findMin(h2))
      Math.min(min1, min2) == findMin(meld(h1, h2))
    }
    else true
  }

  property("findMin of an empty heap throws an exception") = forAll { (i: Int) =>
    throws(classOf[NoSuchElementException])({ findMin(empty) })
  }

  property("deleteMin of an empty heap throws an exception") = forAll { (i: Int) =>
    throws(classOf[NoSuchElementException])({ deleteMin(empty) })
  }

  property("Meld empty to empty gives an empty") =  forAll { (i: Int) =>
    isEmpty(meld(empty, empty))
  }

  property("findMin/deleteMin/findMind etc. of two melded arbitrary heaps is ordered") = forAll { (h1: H, h2: H) =>
    isOrderedRecur(meld(h1, h2), Int.MinValue)
  }

}
