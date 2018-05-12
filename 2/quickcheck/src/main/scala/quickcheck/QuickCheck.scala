package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[Int]
      h <- genHeap
    } yield insert(a, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == min(a, b)
  }

  property("del1") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sorted") = forAll { h: H =>
    isEmpty(h) || {
      def check(a: Int, h2: H): Boolean =
        isEmpty(h2) || {
          val b = findMin(h2)
          a <= b && check(b, deleteMin(h2))
        }
      check(findMin(h), deleteMin(h))
    }
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    isEmpty(h) || {
      val a = findMin(h)
      (!isEmpty(h1) && a == findMin(h1)) || (!isEmpty(h2) && a == findMin(h2))
    }
  }

  property("meld2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(b, empty)
    val h2 = insert(a, empty)
    val h = meld(h1, h2)
    findMin(deleteMin(h)) == max(a, b)
  }

  property("deleteMin") = forAll { (a: Int, b: Int, c: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = insert(c, h2)
    val h4 = deleteMin(h3)
    val h5 = deleteMin(h4)
    findMin(h5) == max(a, max(b, c))
  }
}
