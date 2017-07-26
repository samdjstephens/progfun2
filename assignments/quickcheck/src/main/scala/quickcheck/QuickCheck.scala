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

  def isOrdered(h: H): Boolean = {
    def isOrderedRecur(h: H, prevMin: Int): Boolean = {
      if (isEmpty(h)) true
      else if (findMin(h) < prevMin) false
      else isOrderedRecur(deleteMin(h), findMin(h))
    }
    isOrderedRecur(h, Int.MinValue)
  }

  def contains(h: H, x: Int): Boolean = {
    if (isEmpty(h)) false
    else if (findMin(h) == x) true
    else contains(deleteMin(h), x)
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
    isOrdered(h)
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

  property("meld or two arbitrary heaps is ordered") = forAll { (h1: H, h2: H) =>
    isOrdered(meld(h1, h2))
  }

  property("insert arb element into arb heap is ordered") = forAll { (h: H, i: Int) =>
    isOrdered(insert(i, h))
  }

  property("meld of empty and arbitrary heap is ordered") = forAll { (h: H) =>
    isOrdered(meld(empty, h))
  }

  property("empty with 3 elements inserted is ordered") = forAll { (i: Int, j: Int, k: Int) =>
    val h = insert(k, insert(j, insert(i, empty)))
    isOrdered(h)
  }

  property("findMin after insert of larger-than-min element into arb heap is previous min") = forAll { (h: H) =>
    if (!isEmpty(h) && findMin(h) < Int.MaxValue) {
      val min = findMin(h)
      findMin(insert(min+1, h)) == min
    } else true
  }

  property("findMin after insert of smaller than min element into arb heap is new element") = forAll { (h: H) =>
    if (!isEmpty(h) && findMin(h) > Int.MinValue) {
      val min = findMin(h)
      val newMin = min - 1
      findMin(insert(newMin, h)) == newMin
    } else true
  }

  property("findMin of 1 element min value heap melded with arbitrary heap is min value") = forAll { (h: H) =>
    val minValueHeap = insert(Int.MinValue, empty)
    findMin(meld(h, minValueHeap)) == Int.MinValue
  }

  property("order of meld of two arbitrary heap doesnt impact findMin") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2))
      findMin(meld(h1, h2)) == findMin(meld(h2, h1))
    else true
  }

  property("heap resulting from inserting arbitrary element contains element") = forAll { (h: H, i: Int) =>
    val derived = insert(i, h)
    contains(derived, i)
  }
}
