// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import stream00._    // uncomment to test the book solution (should pass your tests)
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

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

    "it should not force the tail of the stream" in {
      forAll { (n: Int) =>
        cons (n, cons(throw new RuntimeException("it forced the tail!"), Stream.empty)).headOption should be (Some(n))
      }
    }
  }

  "take" - {

    // Exercise 3
    "should not force any heads nor any tails of the streams it manipulates" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // Uses our generator of non empty streams
      // thanks to the implicit declaration above
      forAll { (s: Stream[Int], n: Int) =>
        s.map(_ =>
          throw new RuntimeException("it forced the tail!"))
          .take(n)
      }
    }

    // Exercise 4
    "does not force the (n+1)st head ever (even if we force all elements of take(n))" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s :Stream[Int]) => {
        val x = s.toList.size
        val s2 = s.append(cons(???, ???))
        s2.take(x).toList
      } }
    }

    // Exercise 5
    "s.take(n).take(n) == s.take(n) for any stream s and any n" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll{ (s :Stream[Int], n: Int) => {
        s.take(n).take(n).toList shouldBe s.take(n).toList
        }
      }
    }

  }

  "drop" - {

    // Exercise 6

    "s.drop(n).drop(m) ==s.drop(n+m) for any n,m" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll{ (s :Stream[Int], n: Int, m: Int) => {
        if (n > 0 && m >0 && Int.MaxValue - n > m) s.drop(n).drop(m).toList shouldBe s.drop(n+m).toList else true
        }
      }
    }

    // Exercise 7

    "s.drop(n) does not force any of the dropped elements heads (should hold even if an element forced in the tail)" in {
      val s = cons(???, cons(1, Stream.empty))

      s.drop(1).toList
    }
  }


  "map" - {

     // Exercise 8
    "x.map(id) == x for any stream" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s: Stream[Int]) => {
        s.map(identity).toList shouldBe(s.toList)
      }}
    }

     // Exercise 9
    "map terminates on infinite streams" in {
      Stream.from(1).map(_ + 1)
    }

  }

  "append" - {

    // Exercise 10

    "append two empty streams end up in empty stream" in {
      Stream.empty.append(Stream.empty).toList shouldBe(Stream.empty.toList)
    }

    "append empty stream results in the original stream" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s: Stream[Int]) => {
        s.append(Stream.empty).toList shouldBe(s.toList)
        }
      }
    }

    "appends two streams" in {
      val s1 = Stream(1,2,3,4,5)
      val s2 = Stream(6,7,8,9,10)
      s1.append(s2).toList shouldBe(Stream(1,2,3,4,5, 6,7,8,9,10).toList)
    }

    "head of the stream after append is the same as the original head" in {
      implicit val arbIntStream =
        Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s1: Stream[Int], s2: Stream[Int]) => {
        val s = s1.append(s2)
        s.headOption shouldBe(s1.headOption)
        }
      }
    }

  }

}
