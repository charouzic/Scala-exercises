// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import stream00._    // uncomment to test the book solution (should pass your tests)
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams

  def list2stream[A] (la: List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.

  def genNonEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // Exercise 1 (no coding, understand)

    // A scenario test:

    "returns None on an empty Stream (01)" in {

      Stream.empty.headOption shouldBe (None)
    }


    // Two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {

      forAll { (n: Int) =>
        cons (n, Stream.empty).headOption should be (Some (n))
      }
    }

    "returns the head of random stream packaged in Some (02)" in {

      // Make the generator available in the context
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      forAll { (s: Stream[Int]) =>
        s.headOption shouldNot be (None)
      }
    }

    // Exercise 2 (add here)
    //
    // ...

  }

  "take" - {

    // Exercise 3

    // Exercise 4

    // Exercise 5

  }

  "drop" - {

    // Exercise 6

    // Exercise 7

  }


  "map" - {

     // Exercise 8

     // Exercise 9

  }

  "append" - {

    // Exercise 10

  }

}
